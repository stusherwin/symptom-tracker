module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Page.Chartables as ChartablesPage
import Page.Charts as ChartsPage
import Page.Day as DayPage
import Page.Trackables as TrackablesPage
import Ports exposing (setUserData)
import Svg.Icon exposing (IconType(..), icon, iconSymbols, logo)
import Task
import Throttle exposing (Throttle)
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)
import UserData exposing (UserData)
import UserData.LineChartId exposing (LineChartId(..))



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
    | Trackables
    | Chartables
    | Charts (Maybe LineChartId)


monthParser : Parser (Month -> a) a
monthParser =
    Parser.custom "MONTH" (String.toInt >> Maybe.map Date.numberToMonth)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Today top
        , Parser.map Day (Parser.map Date.fromCalendarDate (Parser.s "day" </> Parser.int </> monthParser </> Parser.int))
        , Parser.map Trackables (Parser.s "trackables")
        , Parser.map Chartables (Parser.s "chartables")
        , Parser.map Charts (Parser.s "charts" </> Parser.map (Just << LineChartId) Parser.int)
        , Parser.map (Charts Nothing) (Parser.s "charts")
        ]


routeToPage : Date -> UserData -> Maybe Route -> Nav.Key -> ( Page, Cmd Msg )
routeToPage today userData route navKey =
    case route of
        Just Today ->
            ( DayPage <| DayPage.init today today userData, Cmd.none )

        Just (Day date) ->
            ( DayPage <| DayPage.init today date userData, Cmd.none )

        Just Trackables ->
            ( TrackablesPage <| TrackablesPage.init userData, Cmd.none )

        Just Chartables ->
            ( ChartablesPage <| ChartablesPage.init userData, Cmd.none )

        Just (Charts chartId) ->
            let
                ( model, cmd ) =
                    ChartsPage.init today userData navKey chartId
            in
            ( ChartsPage model, Cmd.map ChartsPageMsg cmd )

        _ ->
            ( NotFoundPage, Cmd.none )



-- MODEL


type alias Model =
    { pageState : PageState
    , navKey : Nav.Key
    , throttle : Throttle Msg
    }


type PageState
    = Loading (Maybe Route) (Maybe Date) (Maybe UserData)
    | Error String
    | Loaded Date UserData Page


type Page
    = DayPage DayPage.Model
    | TrackablesPage TrackablesPage.Model
    | ChartablesPage ChartablesPage.Model
    | ChartsPage ChartsPage.Model
    | NotFoundPage


init : Encode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { pageState =
            case Decode.decodeValue UserData.decode flags of
                Ok userData ->
                    Loading (Parser.parse routeParser url) Nothing (Just userData)

                Err err ->
                    -- Loading (Parser.parse routeParser url) Nothing (Just Trackable.init)
                    Error <| Decode.errorToString err
      , navKey = key
      , throttle = Throttle.create 1
      }
    , Task.perform GotCurrentDate Date.today
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Throttle.ifNeeded
            (Time.every 1000 (\_ -> UpdateThrottle))
            model.throttle
        , case model.pageState of
            Loaded _ _ (ChartsPage page) ->
                Sub.map ChartsPageMsg (ChartsPage.subscriptions page)

            -- Loaded _ _ (ChartPage page) ->
            --     Sub.map ChartPageMsg (ChartPage.subscriptions page)
            _ ->
                Sub.none
        ]



-- UPDATE


type Msg
    = GotCurrentDate Date
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | DayPageMsg DayPage.Msg
    | ChartsPageMsg ChartsPage.Msg
    | TrackablesPageMsg TrackablesPage.Msg
    | ChartablesPageMsg ChartablesPage.Msg
    | UpdateThrottle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateUserData userData_ =
            case model.pageState of
                Loaded today _ page ->
                    let
                        ( newThrottle, cmd ) =
                            Throttle.try (setUserData <| UserData.encode <| userData_) model.throttle
                    in
                    ( { model | throttle = newThrottle, pageState = Loaded today userData_ page }, cmd )

                _ ->
                    ( model, Cmd.none )
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

                Loaded today userData pageState ->
                    case ( pageState, route ) of
                        ( ChartsPage chartsPageModel, Just (Charts chartId) ) ->
                            let
                                ( page, cmd ) =
                                    ChartsPage.urlChanged userData chartId chartsPageModel
                            in
                            ( { model | pageState = Loaded today userData (ChartsPage page) }, Cmd.map ChartsPageMsg cmd )

                        _ ->
                            let
                                ( page, cmd ) =
                                    routeToPage today userData route model.navKey
                            in
                            ( { model | pageState = Loaded today userData page }, cmd )

        GotCurrentDate today ->
            case model.pageState of
                Loading route _ Nothing ->
                    ( { model | pageState = Loading route (Just today) Nothing }, Cmd.none )

                Loading route _ (Just userData) ->
                    let
                        ( page, cmd ) =
                            routeToPage today userData route model.navKey
                    in
                    ( { model | pageState = Loaded today userData page }, cmd )

                _ ->
                    ( model, Cmd.none )

        DayPageMsg (DayPage.UserDataUpdated userData_) ->
            updateUserData userData_

        DayPageMsg dayPageMsg ->
            case model.pageState of
                Loaded today userData (DayPage dayPageModel) ->
                    let
                        ( newModel, cmd ) =
                            DayPage.update userData dayPageMsg dayPageModel
                    in
                    ( { model | pageState = Loaded today userData (DayPage newModel) }, Cmd.map DayPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        ChartsPageMsg (ChartsPage.UserDataUpdated userData_) ->
            updateUserData userData_

        ChartsPageMsg chartsPageMsg ->
            case model.pageState of
                Loaded today userData (ChartsPage chartsPageModel) ->
                    let
                        ( newModel, cmd ) =
                            ChartsPage.update userData chartsPageMsg chartsPageModel
                    in
                    ( { model | pageState = Loaded today userData (ChartsPage newModel) }, Cmd.map ChartsPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        TrackablesPageMsg (TrackablesPage.UserDataUpdated userData_) ->
            updateUserData userData_

        TrackablesPageMsg trackablesPageMsg ->
            case model.pageState of
                Loaded today userData (TrackablesPage trackablesPageModel) ->
                    let
                        ( newModel, cmd ) =
                            TrackablesPage.update userData trackablesPageMsg trackablesPageModel
                    in
                    ( { model | pageState = Loaded today userData (TrackablesPage newModel) }, Cmd.map TrackablesPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        ChartablesPageMsg (ChartablesPage.UserDataUpdated userData_) ->
            updateUserData userData_

        ChartablesPageMsg chartablesPageMsg ->
            case model.pageState of
                Loaded today userData (ChartablesPage chartablesPageModel) ->
                    let
                        ( newModel, cmd ) =
                            ChartablesPage.update userData chartablesPageMsg chartablesPageModel
                    in
                    ( { model | pageState = Loaded today userData (ChartablesPage newModel) }, Cmd.map ChartablesPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

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
                [ class "mobile-width mx-auto min-h-screen relative bg-gray-300" ]
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

                                    TrackablesPage trackablesModel ->
                                        Html.map TrackablesPageMsg <| TrackablesPage.view trackablesModel

                                    ChartablesPage chartablesModel ->
                                        Html.map ChartablesPageMsg <| ChartablesPage.view chartablesModel

                                    ChartsPage graphModel ->
                                        Html.map ChartsPageMsg <| ChartsPage.view graphModel

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
        , a [ href "/charts", class "mr-2 rounded p-2 bg-gray-700 hover:bg-gray-600 text-white" ] [ icon "w-5 h-5" SolidChartArea ]
        , a [ href "/trackables", class "mr-2 rounded p-2 bg-gray-700 hover:bg-gray-600 text-white" ] [ icon "w-5 h-5" SolidCalendarCheck ]
        , a [ href "/chartables", class "rounded p-2 bg-gray-700 hover:bg-gray-600 text-white" ] [ icon "w-5 h-5" SolidChartLine ]
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
