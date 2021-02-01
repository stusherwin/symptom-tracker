port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (preventDefaultOn, stopPropagationOn)
import Icon exposing (IconType(..), icon, iconSymbols, logo)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Page.Day as DayPage
import Page.Graph as GraphPage
import Page.Settings as SettingsPage
import Task
import Throttle exposing (Throttle)
import Time exposing (Month(..))
import Trackable exposing (TrackableData(..))
import Trackables exposing (Trackables)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)



-- APP


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- ROUTE


type Route
    = Today
    | Day Date
    | Settings
    | Graph


monthParser : Parser (Month -> a) a
monthParser =
    Parser.custom "MONTH" (String.toInt >> Maybe.map Date.numberToMonth)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Today top
        , Parser.map Day (Parser.map Date.fromCalendarDate (Parser.s "day" </> Parser.int </> monthParser </> Parser.int))
        , Parser.map Settings (Parser.s "settings")
        , Parser.map Graph (Parser.s "graph")
        ]


routeToPage : Date -> Trackables -> Maybe Route -> ( Page, Cmd Msg )
routeToPage today trackables route =
    case route of
        Just Today ->
            ( DayPage <| DayPage.init today today trackables, Cmd.none )

        Just (Day date) ->
            ( DayPage <| DayPage.init today date trackables, Cmd.none )

        Just Settings ->
            ( SettingsPage <| SettingsPage.init trackables, Cmd.none )

        Just Graph ->
            let
                ( model, cmd ) =
                    GraphPage.init today trackables
            in
            ( GraphPage model, Cmd.map GraphPageMsg cmd )

        _ ->
            ( NotFoundPage, Cmd.none )



-- MODEL


type alias Model =
    { pageState : PageState
    , navKey : Nav.Key
    , throttle : Throttle Msg
    }


type PageState
    = Loading (Maybe Route) (Maybe Date) (Maybe Trackables)
    | Error String
    | Loaded Date Trackables Page


type Page
    = DayPage DayPage.Model
    | SettingsPage SettingsPage.Model
    | GraphPage GraphPage.Model
    | NotFoundPage


init : Encode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { pageState =
            case Decode.decodeValue Trackables.decode flags of
                Ok trackables ->
                    Loading (Parser.parse routeParser url) Nothing (Just trackables)

                Err err ->
                    -- Loading (Parser.parse routeParser url) Nothing (Just Trackable.init)
                    Error <| Decode.errorToString err
      , navKey = key
      , throttle = Throttle.create 1
      }
    , Task.perform GotCurrentDate Date.today
    )



-- PORTS


port setTrackables : Encode.Value -> Cmd msg


port onTrackablesChange : (Encode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onTrackablesChange TrackablesChanged
        , Throttle.ifNeeded
            (Time.every 1000 (\_ -> UpdateThrottle))
            model.throttle
        ]



-- UPDATE


type Msg
    = GotCurrentDate Date
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | DayPageMsg DayPage.Msg
    | GraphPageMsg GraphPage.Msg
    | SettingsPageMsg SettingsPage.Msg
    | TrackablesChanged Encode.Value
    | UpdateThrottle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateTrackables result =
            case result of
                Ok newTrackables ->
                    let
                        ( newThrottle, cmd ) =
                            Throttle.try (setTrackables <| Trackables.encode <| newTrackables) model.throttle
                    in
                    ( { model | throttle = newThrottle }, cmd )

                Err err ->
                    ( { model | pageState = Error err }, Cmd.none )
    in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Parser.parse routeParser url
            in
            case model.pageState of
                Loading _ d t ->
                    ( { model | pageState = Loading route d t }, Cmd.none )

                Error err ->
                    ( { model | pageState = Error err }, Cmd.none )

                Loaded today trackables _ ->
                    let
                        ( page, cmd ) =
                            routeToPage today trackables route
                    in
                    ( { model | pageState = Loaded today trackables page }, cmd )

        GotCurrentDate today ->
            case model.pageState of
                Loading route _ Nothing ->
                    ( { model | pageState = Loading route (Just today) Nothing }, Cmd.none )

                Loading route _ (Just trackables) ->
                    let
                        ( page, cmd ) =
                            routeToPage today trackables route
                    in
                    ( { model | pageState = Loaded today trackables page }, cmd )

                _ ->
                    ( model, Cmd.none )

        DayPageMsg (DayPage.UpdateTrackable fn id) ->
            case model.pageState of
                Loaded _ trackables _ ->
                    updateTrackables <| Trackables.update id fn trackables

                _ ->
                    ( model, Cmd.none )

        DayPageMsg dayPageMsg ->
            case model.pageState of
                Loaded today trackables (DayPage dayPageModel) ->
                    let
                        ( newModel, cmd ) =
                            DayPage.update dayPageMsg dayPageModel
                    in
                    ( { model | pageState = Loaded today trackables (DayPage newModel) }, Cmd.map DayPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        GraphPageMsg graphPageMsg ->
            case model.pageState of
                Loaded today trackables (GraphPage graphPageModel) ->
                    let
                        ( newModel, cmd ) =
                            GraphPage.update graphPageMsg graphPageModel
                    in
                    ( { model | pageState = Loaded today trackables (GraphPage newModel) }, Cmd.map GraphPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        SettingsPageMsg (SettingsPage.UpdateTrackable fn id) ->
            case model.pageState of
                Loaded _ trackables _ ->
                    updateTrackables <| Trackables.update id fn trackables

                _ ->
                    ( model, Cmd.none )

        SettingsPageMsg (SettingsPage.AddTrackable t id) ->
            case model.pageState of
                Loaded _ trackables _ ->
                    updateTrackables <| Trackables.add id t trackables

                _ ->
                    ( model, Cmd.none )

        SettingsPageMsg (SettingsPage.DeleteTrackable id) ->
            case model.pageState of
                Loaded _ trackables _ ->
                    updateTrackables <| Trackables.delete id trackables

                _ ->
                    ( model, Cmd.none )

        SettingsPageMsg settingsPageMsg ->
            case model.pageState of
                Loaded today trackables (SettingsPage settingsPageModel) ->
                    let
                        ( newModel, cmd ) =
                            SettingsPage.update settingsPageMsg settingsPageModel
                    in
                    ( { model | pageState = Loaded today trackables (SettingsPage newModel) }, Cmd.map SettingsPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        TrackablesChanged data ->
            case Decode.decodeValue Trackables.decode data of
                Ok trackables ->
                    case model.pageState of
                        Loading route d _ ->
                            ( { model | pageState = Loading route d (Just trackables) }, Cmd.none )

                        Loaded today _ (GraphPage graphPageModel) ->
                            let
                                ( newModel, cmd ) =
                                    GraphPage.update (GraphPage.TrackablesChanged trackables) graphPageModel
                            in
                            ( { model | pageState = Loaded today trackables (GraphPage newModel) }
                            , Cmd.map GraphPageMsg cmd
                            )

                        Loaded today _ (DayPage dayPageModel) ->
                            let
                                ( newModel, cmd ) =
                                    DayPage.update (DayPage.TrackablesChanged trackables) dayPageModel
                            in
                            ( { model | pageState = Loaded today trackables (DayPage newModel) }
                            , Cmd.map DayPageMsg cmd
                            )

                        Loaded today _ page ->
                            ( { model | pageState = Loaded today trackables page }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    ( { model | pageState = Error (Decode.errorToString err) }, Cmd.none )

        UpdateThrottle ->
            let
                ( newThrottle, cmd ) =
                    Throttle.update model.throttle
            in
            ( { model | throttle = newThrottle }, cmd )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Symptom Tracker"
    , body =
        [ iconSymbols
        , div [ class "min-h-screen bg-gray-600" ]
            [ div
                [ class "mobile-width mx-auto min-h-screen relative bg-white" ]
                [ viewMenu model.pageState
                , div [ class "flex flex-col items-stretch" ] <|
                    case model.pageState of
                        Loading _ _ _ ->
                            [ viewLoading ]

                        Error err ->
                            [ viewError err ]

                        Loaded _ _ page ->
                            [ div []
                                [ case page of
                                    DayPage dayModel ->
                                        Html.map DayPageMsg <| DayPage.view dayModel

                                    SettingsPage settingsModel ->
                                        Html.map SettingsPageMsg <| SettingsPage.view settingsModel

                                    GraphPage graphModel ->
                                        Html.map GraphPageMsg <| GraphPage.view graphModel

                                    NotFoundPage ->
                                        viewNotFoundPage
                                ]
                            ]
                ]
            ]
        ]
    }


viewMenu : PageState -> Html Msg
viewMenu pageState =
    div [ class "px-4 bg-gray-800 text-white flex items-center h-16" ]
        [ logo "w-8 h-8"
        , h1 [ class "ml-2 text-xl font-bold text-center" ] [ text "Symptrack" ]
        , div [ class "mx-auto" ] []
        , a [ href "/", class "mr-2 rounded p-2 bg-gray-700 hover:bg-gray-600 text-white" ] [ icon "w-5 h-5" SolidCalendarAlt ]
        , a [ href "/graph", class "mr-2 rounded p-2 bg-gray-700 hover:bg-gray-600 text-white" ] [ icon "w-5 h-5" SolidChartLine ]
        , a [ href "/settings", class "rounded p-2 bg-gray-700 hover:bg-gray-600 text-white" ] [ icon "w-5 h-5" SolidCog ]
        ]


viewLoading : Html Msg
viewLoading =
    div []
        [ h1 [ class "font-bold text-3xl text-center" ]
            [ text "Loading..." ]
        ]


viewError : String -> Html Msg
viewError error =
    div []
        [ h1 [ class "font-bold text-3xl text-center" ]
            [ text <| "Error: " ++ error ]
        ]


viewNotFoundPage : Html Msg
viewNotFoundPage =
    div []
        [ h1 [ class "font-bold text-3xl text-center" ]
            [ text "Page not found" ]
        ]


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    let
        alwaysPreventDefault m =
            ( m, True )
    in
    preventDefaultOn "click" (Decode.map alwaysPreventDefault (Decode.succeed msg))


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    let
        alwaysStopPropagation m =
            ( m, True )
    in
    stopPropagationOn "click" (Decode.map alwaysStopPropagation (Decode.succeed msg))
