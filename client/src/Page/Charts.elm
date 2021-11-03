module Page.Charts exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Dom as Dom
import Colour exposing (Colour)
import Controls
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Dictx
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onMouseEnter, onMouseLeave)
import Htmlx
import IdDict
import Listx
import Maybe exposing (Maybe)
import Ports exposing (fullScreenChanged, toggleElementFullScreen)
import Stringx
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), fillIcon, icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as Chartable exposing (Chartable, ChartableDict)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChart as LineChart exposing (LineChart)
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..), TrackableDict)
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Model =
    { today : Date
    , charts : List ( LineChartId, Graph.Model LineChartModel ChartableId ChartableModel )
    , userData : UserData
    , trackableOptions : List ( TrackableId, ( String, Bool ) )
    , chartableOptions : List ( ChartableId, String )
    , fullScreen : Bool
    }


type alias LineChartModel =
    { addState : EditState
    , viewport : Maybe Dom.Viewport
    , expandedValue : Bool
    , name : String
    }


type EditState
    = NotAdding
    | AddingChartable (Maybe ChartableId)


type alias ChartableModel =
    { trackables : List ( TrackableId, TrackableModel )
    , inverted : Bool
    , nameIsPristine : Bool
    }


type alias TrackableModel =
    { question : String
    , multiplier : String
    , isValid : Bool
    }


init : Date -> UserData -> ( Model, Cmd Msg )
init today userData =
    ( { today = today
      , charts =
            UserData.lineCharts userData
                |> IdDict.map (\_ chart -> toChartModel today userData chart)
                |> IdDict.toList
      , trackableOptions =
            UserData.activeTrackables userData
                |> List.map (Tuple.mapSecond (Tuple.mapFirst .question))
                |> List.sortBy (String.toUpper << Tuple.first << Tuple.second)
      , chartableOptions =
            UserData.chartables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .name))
                |> List.sortBy (String.toUpper << Tuple.second)
      , userData = userData
      , fullScreen = False
      }
    , Cmd.batch <|
        (UserData.lineCharts userData
            |> IdDict.keys
            |> List.map
                (\id ->
                    let
                        chartId =
                            "chart" ++ LineChartId.toString id ++ "-scrollable"
                    in
                    Dom.getViewportOf chartId
                        |> Task.andThen (\info -> Dom.setViewportOf chartId info.scene.width 0)
                        |> Task.attempt (always NoOp)
                )
        )
            ++ (UserData.lineCharts userData
                    |> IdDict.keys
                    |> List.map
                        (\chartId ->
                            Task.map2 Tuple.pair
                                (Dom.getViewportOf <| "chart" ++ LineChartId.toString chartId ++ "-scrollable")
                                (Dom.getElement <| "chart" ++ LineChartId.toString chartId ++ "-svg")
                                |> Task.attempt (ViewportUpdated chartId)
                        )
               )
    )


toChartModel : Date -> UserData -> LineChart -> Graph.Model LineChartModel ChartableId ChartableModel
toChartModel today userData chart =
    let
        toChartableModel_ ( chartableId, visible ) =
            userData
                |> UserData.getChartable chartableId
                |> Maybe.map (\c -> ( chartableId, toChartableModel userData visible c ))
    in
    { today = today
    , name = chart.name
    , fillLines = chart.fillLines
    , data =
        chart.chartables
            |> List.filterMap toChartableModel_
    , selectedDataSet = Nothing
    , leavingDataSet = Nothing
    , hoveredDataSet = Nothing
    , selectedDataPoint = Nothing
    , hoveredDataPoint = Nothing
    , addState = NotAdding
    , viewport = Nothing
    , expandedValue = False
    , xScale = 1
    , minWidth = 0
    , currentWidth = 0
    }


toChartableModel : UserData -> Bool -> Chartable -> Graph.DataSet ChartableModel
toChartableModel userData visible chartable =
    { name = chartable.name
    , colour = toColour userData chartable
    , dataPoints = toDataPoints userData chartable
    , inverted = chartable.inverted
    , trackables = chartable.sum |> List.filterMap (toTrackableModel userData)
    , visible = visible
    , nameIsPristine = True
    }


toTrackableModel : UserData -> ( TrackableId, Float ) -> Maybe ( TrackableId, TrackableModel )
toTrackableModel userData ( trackableId, multiplier ) =
    userData
        |> UserData.getTrackable trackableId
        |> Maybe.map
            (\{ question } ->
                ( trackableId
                , { question = question
                  , multiplier = String.fromFloat multiplier
                  , isValid = True
                  }
                )
            )


toColour : UserData -> Chartable -> Colour
toColour userData chartable =
    let
        firstColour =
            List.head chartable.sum
                |> Maybe.andThen ((\tId -> UserData.getTrackable tId userData) << Tuple.first)
                |> Maybe.map .colour
    in
    Maybe.withDefault Colour.Gray <|
        if List.length chartable.sum == 1 then
            firstColour

        else
            case chartable.colour of
                Just c ->
                    Just c

                Nothing ->
                    firstColour


toDataPoints : UserData -> Chartable -> Dict Int Float
toDataPoints userData chartable =
    let
        invert data =
            case List.maximum <| Dict.values data of
                Just max ->
                    data |> Dict.map (\_ v -> max - v)

                _ ->
                    data
    in
    chartable.sum
        |> List.filterMap
            (\( trackableId, multiplier ) ->
                userData
                    |> UserData.getTrackable trackableId
                    |> Maybe.map
                        (Dict.map (\_ v -> v * multiplier) << Trackable.onlyFloatData)
            )
        |> List.foldl (Dictx.unionWith (\v1 v2 -> v1 + v2)) Dict.empty
        |> (if chartable.inverted then
                invert

            else
                identity
           )


subscriptions : Model -> Sub Msg
subscriptions _ =
    fullScreenChanged FullScreenChanged



-- UPDATE


type Msg
    = NoOp
    | ChartFillLinesChecked LineChartId Bool
    | ChartShowPointsChecked LineChartId Bool
    | ChartFullScreenClicked LineChartId
    | ChartZoomOutClicked LineChartId
    | ChartZoomInClicked LineChartId
    | ChartClicked LineChartId
    | ChartExpandValueClicked LineChartId
    | ChartExpandValueCloseClicked LineChartId
    | ChartAddClicked
    | FullScreenChanged Bool
    | ViewportUpdated LineChartId (Result Dom.Error ( Dom.Viewport, Dom.Element ))
    | UserDataUpdated UserData
    | GraphMsg LineChartId (Graph.Msg ChartableId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChartModel chartId fn m =
            { m | charts = m.charts |> Listx.updateLookup chartId fn }

        setUserData userData_ m =
            { m | userData = userData_ }
    in
    case msg of
        ChartFillLinesChecked chartId fl ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.setFillLines fl)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| \c -> { c | fillLines = fl })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartFullScreenClicked chartId ->
            ( model, toggleElementFullScreen ("chart" ++ LineChartId.toString chartId) )

        ChartClicked chartId ->
            ( model |> updateChartModel chartId (\c -> { c | selectedDataSet = Nothing, selectedDataPoint = Nothing }), Cmd.none )

        ChartZoomOutClicked chartId ->
            ( model |> updateChartModel chartId (\c -> { c | xScale = c.xScale * 3 / 4 })
            , Task.map2 Tuple.pair
                (Dom.getViewportOf <| "chart" ++ LineChartId.toString chartId ++ "-scrollable")
                (Dom.getElement <| "chart" ++ LineChartId.toString chartId ++ "-svg")
                |> Task.attempt (ViewportUpdated chartId)
            )

        ChartZoomInClicked chartId ->
            ( model |> updateChartModel chartId (\c -> { c | xScale = c.xScale * 4 / 3 })
            , Task.map2 Tuple.pair
                (Dom.getViewportOf <| "chart" ++ LineChartId.toString chartId ++ "-scrollable")
                (Dom.getElement <| "chart" ++ LineChartId.toString chartId ++ "-svg")
                |> Task.attempt (ViewportUpdated chartId)
            )

        ChartExpandValueClicked chartId ->
            ( model
                |> (updateChartModel chartId <| \c -> { c | expandedValue = not c.expandedValue })
            , Cmd.none
            )

        ChartExpandValueCloseClicked chartId ->
            ( model |> (updateChartModel chartId <| Graph.selectDataSet Nothing << (\c -> { c | expandedValue = False })), Cmd.none )

        FullScreenChanged fullScreen ->
            ( { model | fullScreen = fullScreen }, Cmd.none )

        ViewportUpdated chartId (Ok ( scrollable, svg )) ->
            ( model |> (updateChartModel chartId <| \c -> { c | viewport = Just scrollable, currentWidth = svg.element.width, minWidth = scrollable.viewport.width }), Cmd.none )

        GraphMsg chartId (Graph.MouseDown ( x, y )) ->
            ( model
                |> (updateChartModel chartId <|
                        \c ->
                            case c.viewport of
                                Just { scene } ->
                                    let
                                        xPerc =
                                            x / scene.width

                                        yPerc =
                                            y / scene.height
                                    in
                                    c |> Graph.selectNearestDataPoint ( xPerc, yPerc )

                                -- { c | point = Just ( xPerc, yPerc ) }
                                _ ->
                                    c
                   )
            , Cmd.none
            )

        GraphMsg chartId (Graph.MouseMove ( x, y )) ->
            ( model
                |> (updateChartModel chartId <|
                        \c ->
                            case c.viewport of
                                Just { scene } ->
                                    let
                                        xPerc =
                                            x / scene.width

                                        yPerc =
                                            y / scene.height
                                    in
                                    c |> Graph.hoverNearestDataPoint ( xPerc, yPerc )

                                _ ->
                                    c
                   )
            , Cmd.none
            )

        GraphMsg chartId graphMsg ->
            ( model |> (updateChartModel chartId <| Graph.update graphMsg), Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "bg-white"
        ]
    <|
        h2 [ class "mb-4 py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
            :: (model.charts
                    |> List.map (viewLineChart model.fullScreen model.chartableOptions model.userData)
               )
            ++ [ div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                    [ Controls.button "" Controls.ButtonGrey ChartAddClicked SolidPlusCircle "Add new chart" True
                    ]
               ]


viewLineChart : Bool -> List ( ChartableId, String ) -> UserData -> ( LineChartId, Graph.Model LineChartModel ChartableId ChartableModel ) -> Html Msg
viewLineChart fullScreen chartableOptions userData ( chartId, model ) =
    div
        []
        [ div [ class "mb-2 px-4" ]
            [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black", href <| "/charts/" ++ LineChartId.toString chartId ]
                [ span [] [ text <| model.name ]
                , icon "absolute right-0 w-5 h-5" SolidPencilAlt
                ]
            ]
        , div
            [ id ("chart" ++ LineChartId.toString chartId)
            , class "mb-8 bg-white"
            , classList
                [ ( "p-8", fullScreen )
                ]
            ]
            [ div
                [ class "mr-4 my-0 flex scrollable-parent relative"
                , style "height" "300px"
                ]
                ([ viewJustYAxis "flex-grow-0 flex-shrink-0" model
                 , viewScrollableContainer ("chart" ++ LineChartId.toString chartId ++ "-scrollable") [ Html.map (GraphMsg chartId) <| viewLineGraph ("chart" ++ LineChartId.toString chartId ++ "-svg") "h-full" model ]
                 , div [ class "absolute right-2 top-6 flex flex-col" ]
                    [ button
                        [ class "rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none text-opacity-70 hover:bg-opacity-100 hover:text-opacity-100 focus:text-opacity-100"
                        , Htmlx.onClickStopPropagation (ChartFullScreenClicked chartId)
                        ]
                        [ icon "w-5 h-5" <|
                            if fullScreen then
                                SolidCompress

                            else
                                SolidExpand
                        ]
                    , button
                        [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none text-opacity-70 hover:bg-opacity-100 hover:text-opacity-100 focus:text-opacity-100"
                        , Htmlx.onClickStopPropagation (ChartZoomInClicked chartId)
                        ]
                        [ icon "w-5 h-5" SolidPlus
                        ]
                    , button
                        [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none"
                        , classList
                            [ ( "text-opacity-50 cursor-default", model.currentWidth <= model.minWidth )
                            , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", model.currentWidth > model.minWidth )
                            ]
                        , Htmlx.onClickStopPropagation (ChartZoomOutClicked chartId)
                        , disabled (model.currentWidth <= model.minWidth)
                        ]
                        [ icon "w-5 h-5" <| SolidMinus
                        ]
                    , button
                        [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none fill-icon text-opacity-70 hover:bg-opacity-100 focus:bg-opacity:100 focus:text-opacity-100"
                        , Htmlx.onClickStopPropagation (ChartFillLinesChecked chartId <| not model.fillLines)
                        ]
                        [ fillIcon "w-5 h-5" model.fillLines ]
                    ]
                 ]
                    ++ (case model.selectedDataSet |> Maybe.andThen (\id -> Listx.findBy Tuple.first id chartableOptions) of
                            Just ( id, name ) ->
                                [ div
                                    [ class "absolute left-14 top-6 rounded bg-white bg-opacity-80 p-2 min-w-44 max-w-xs" ]
                                    ([ button
                                        [ class "absolute right-2 top-2 text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , Htmlx.onClickStopPropagation (ChartExpandValueCloseClicked chartId)
                                        ]
                                        [ icon "w-4 h-4" SolidTimes
                                        ]
                                     , h4 [ class "font-bold pr-5" ] [ text name ]
                                     ]
                                        ++ (let
                                                featuredDataPoint =
                                                    case model.selectedDataPoint of
                                                        Just p ->
                                                            Just p

                                                        _ ->
                                                            model.hoveredDataPoint
                                            in
                                            case featuredDataPoint of
                                                Just date ->
                                                    let
                                                        data =
                                                            userData
                                                                |> UserData.getChartable id
                                                                |> Maybe.map
                                                                    (.sum
                                                                        >> List.filterMap
                                                                            (\( tId, m ) ->
                                                                                userData |> UserData.getTrackable tId |> Maybe.map (\t -> ( t.question, Maybe.withDefault 0 <| Dict.get date <| Trackable.onlyFloatData t, m ))
                                                                            )
                                                                    )
                                                                |> Maybe.withDefault []

                                                        total =
                                                            List.sum <| List.map (\( _, v, m ) -> v * m) <| data

                                                        invertedTotal =
                                                            let
                                                                inverted =
                                                                    userData
                                                                        |> UserData.getChartable id
                                                                        |> Maybe.map .inverted
                                                                        |> Maybe.withDefault False
                                                            in
                                                            if inverted then
                                                                let
                                                                    max =
                                                                        model.data
                                                                            |> Listx.lookup id
                                                                            |> Maybe.andThen (List.maximum << Dict.values << .dataPoints)
                                                                            |> Maybe.withDefault 0
                                                                in
                                                                Just ( max, max - total )

                                                            else
                                                                Nothing
                                                    in
                                                    [ p [ class "mt-2 text-sm" ] [ text <| Date.format "EEE d MMM y" <| Date.fromRataDie date ]
                                                    , p [ class "text-sm flex justify-between items-baseline" ]
                                                        [ span [] <|
                                                            [ text "Value"
                                                            , span [ class "ml-1 font-bold" ] [ text <| String.fromFloat total ]
                                                            ]
                                                                ++ (case invertedTotal of
                                                                        Just ( _, t ) ->
                                                                            [ span [ class "ml-1" ] [ text "(inverted " ]
                                                                            , span [ class "font-bold" ] [ text <| String.fromFloat t ]
                                                                            , span [ class "" ] [ text ")" ]
                                                                            ]

                                                                        _ ->
                                                                            []
                                                                   )
                                                        , a
                                                            [ href "#"
                                                            , target "_self"
                                                            , class "ml-2 text-sm text-blue-600 hover:text-blue-800 underline"
                                                            , Htmlx.onClickPreventDefault (ChartExpandValueClicked chartId)
                                                            ]
                                                          <|
                                                            if model.expandedValue then
                                                                [ text "less", icon "ml-1 w-3 h-3 inline" SolidAngleUp ]

                                                            else
                                                                [ text "more", icon "ml-1 w-3 h-3 inline" SolidAngleDown ]
                                                        ]
                                                    , if model.expandedValue then
                                                        div [ class "mt-2 pr-2 max-h-24 overflow-y-auto text-sm" ]
                                                            [ table [ class "w-full" ] <|
                                                                (data
                                                                    |> List.indexedMap
                                                                        (\i ( question, value, multiplier ) ->
                                                                            tr []
                                                                                [ td [ class "align-baseline" ]
                                                                                    [ icon "w-2 h-2" <|
                                                                                        if i == 0 then
                                                                                            SolidEquals

                                                                                        else
                                                                                            SolidPlus
                                                                                    ]
                                                                                , td [ class "pl-2 align-baseline" ] [ text question ]
                                                                                , td [ class "pl-2 align-baseline text-right" ] [ text <| String.fromFloat value ]
                                                                                , td [ class "pl-1 align-baseline" ] [ icon "w-2 h-2" SolidTimes ]
                                                                                , td [ class "pl-1 align-baseline text-right" ] [ text <| String.fromFloat multiplier ]
                                                                                , td [ class "pl-1 align-baseline " ] [ icon "w-2 h-2" SolidEquals ]
                                                                                , td [ class "pl-1 align-baseline text-right" ] [ text <| String.fromFloat (value * multiplier) ]
                                                                                ]
                                                                        )
                                                                )
                                                                    ++ (case invertedTotal of
                                                                            Just ( max, t ) ->
                                                                                [ tr []
                                                                                    [ td [ class "align-baseline" ] []
                                                                                    , td [ class "pl-2 align-baseline text-right", colspan 5 ] [ text <| "Total:" ]
                                                                                    , td [ class "pl-1 align-baseline text-right font-bold" ] [ text <| String.fromFloat total ]
                                                                                    ]
                                                                                , tr []
                                                                                    [ td [ class "align-baseline" ] []
                                                                                    , td [ class "pl-2 align-baseline text-right", colspan 2 ]
                                                                                        [ span [ class "" ] [ text "Inverted: " ]
                                                                                        , text <| String.fromFloat max ++ " (max value)"
                                                                                        ]
                                                                                    , td [ class "pl-1 align-baseline" ] [ icon "w-2 h-2" SolidMinus ]
                                                                                    , td [ class "pl-1 align-baseline text-right font-bold" ] [ text <| String.fromFloat total ]
                                                                                    , td [ class "pl-1 align-baseline " ] [ icon "w-2 h-2" SolidEquals ]
                                                                                    , td [ class "pl-1 align-baseline text-right font-bold" ] [ text <| String.fromFloat t ]
                                                                                    ]
                                                                                ]

                                                                            _ ->
                                                                                []
                                                                       )
                                                            ]

                                                      else
                                                        div [] []
                                                    ]

                                                _ ->
                                                    [ p [ class "mt-2 text-sm" ] [ text "Hover over or click on ", br [] [], text "a point to see its value" ] ]
                                           )
                                    )
                                ]

                            _ ->
                                []
                       )
                )
            ]
        ]


viewScrollableContainer : String -> List (Html msg) -> Html msg
viewScrollableContainer containerId children =
    div [ class "relative flex-grow" ]
        [ node "scrollable-container"
            [ id containerId
            , class "absolute overflow-x-scroll top-0 left-0 right-0 bottom-0"
            ]
            children
        ]
