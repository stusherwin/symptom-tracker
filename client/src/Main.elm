port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import IdDict
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Page.Charts as ChartsPage
import Page.Day as DayPage
import Page.Settings as SettingsPage
import Svg.Icon exposing (IconType(..), icon, iconSymbols, logo)
import Task
import Throttle exposing (Throttle)
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)
import UserData exposing (UserData)



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


routeToPage : Date -> UserData -> Maybe Route -> ( Page, Cmd Msg )
routeToPage today userData route =
    case route of
        Just Today ->
            ( DayPage <| DayPage.init today today userData, Cmd.none )

        Just (Day date) ->
            ( DayPage <| DayPage.init today date userData, Cmd.none )

        Just Settings ->
            ( SettingsPage <| SettingsPage.init userData, Cmd.none )

        Just Graph ->
            let
                ( model, cmd ) =
                    ChartsPage.init today userData
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
    | SettingsPage SettingsPage.Model
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



-- PORTS


port setUserData : Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Throttle.ifNeeded
            (Time.every 1000 (\_ -> UpdateThrottle))
            model.throttle
        ]



-- UPDATE


type Msg
    = GotCurrentDate Date
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | DayPageMsg DayPage.Msg
    | ChartsPageMsg ChartsPage.Msg
    | SettingsPageMsg SettingsPage.Msg
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

                Loaded today userData _ ->
                    let
                        ( page, cmd ) =
                            routeToPage today userData route
                    in
                    ( { model | pageState = Loaded today userData page }, cmd )

        GotCurrentDate today ->
            case model.pageState of
                Loading route _ Nothing ->
                    ( { model | pageState = Loading route (Just today) Nothing }, Cmd.none )

                Loading route _ (Just userData) ->
                    let
                        ( page, cmd ) =
                            routeToPage today userData route
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
                            DayPage.update dayPageMsg dayPageModel
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
                            ChartsPage.update chartsPageMsg chartsPageModel
                    in
                    ( { model | pageState = Loaded today userData (ChartsPage newModel) }, Cmd.map ChartsPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        SettingsPageMsg (SettingsPage.UserDataUpdated userData_) ->
            updateUserData userData_

        SettingsPageMsg settingsPageMsg ->
            case model.pageState of
                Loaded today userData (SettingsPage settingsPageModel) ->
                    let
                        ( newModel, cmd ) =
                            SettingsPage.update settingsPageMsg settingsPageModel
                    in
                    ( { model | pageState = Loaded today userData (SettingsPage newModel) }, Cmd.map SettingsPageMsg cmd )

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

                                    SettingsPage settingsModel ->
                                        Html.map SettingsPageMsg <| SettingsPage.view settingsModel

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
