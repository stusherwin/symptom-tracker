module Chart.LineChart exposing (Model, Msg, addDataSet, init, removeDataSet, subscriptions, update, updateColour, updateDataPoints, view)

import Browser.Dom as Dom
import Colour exposing (Colour)
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Dictx
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onMouseEnter, onMouseLeave)
import Htmlx
import Listx
import Maybe exposing (Maybe)
import Ports exposing (fullScreenChanged, toggleElementFullScreen)
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
    { chartId : LineChartId
    , name : String
    , viewport : Maybe Dom.Viewport
    , expandedValue : Bool
    , userData : UserData
    , fullScreen : Bool
    , graph : Graph.Model ChartableId
    , chartables : List ( ChartableId, Bool )
    }


type Msg
    = NoOp
    | ChartFillLinesChecked Bool
    | ChartFullScreenClicked
    | ChartZoomOutClicked
    | ChartZoomInClicked
    | ChartClicked
    | ChartExpandValueClicked
    | ChartExpandValueCloseClicked
    | FullScreenChanged Bool
    | ViewportUpdated (Result Dom.Error ( Dom.Viewport, Dom.Element ))
    | UserDataUpdated UserData
    | GraphMsg (Graph.Msg ChartableId)


init : Date -> UserData -> LineChartId -> LineChart -> ( Model, Cmd Msg )
init today userData chartId chart =
    ( { chartId = chartId
      , name = chart.name
      , viewport = Nothing
      , expandedValue = False
      , userData = userData
      , fullScreen = False
      , graph =
            { today = today
            , fillLines = chart.fillLines
            , data =
                chart.chartables
                    |> List.filterMap
                        (\( chartableId, visible ) -> toDataSet userData chartableId visible)
            , selectedDataSet = Nothing
            , leavingDataSet = Nothing
            , hoveredDataSet = Nothing
            , selectedDataPoint = Nothing
            , hoveredDataPoint = Nothing
            , xScale = 1
            , minWidth = 0
            , currentWidth = 0
            }
      , chartables =
            chart.chartables
                |> List.filterMap
                    (\( chartableId, _ ) ->
                        userData
                            |> UserData.getChartable chartableId
                            |> Maybe.map (\c -> ( chartableId, c.inverted ))
                    )
      }
    , Cmd.batch
        [ let
            elementId =
                "chart" ++ LineChartId.toString chartId ++ "-scrollable"
          in
          Dom.getViewportOf elementId
            |> Task.andThen (\info -> Dom.setViewportOf elementId info.scene.width 0)
            |> Task.attempt (always NoOp)
        , Task.map2 Tuple.pair
            (Dom.getViewportOf <| "chart" ++ LineChartId.toString chartId ++ "-scrollable")
            (Dom.getElement <| "chart" ++ LineChartId.toString chartId ++ "-svg")
            |> Task.attempt ViewportUpdated
        ]
    )


toDataSet : UserData -> ChartableId -> Bool -> Maybe ( ChartableId, Graph.DataSet )
toDataSet userData chartableId visible =
    userData
        |> UserData.getChartable chartableId
        |> Maybe.map
            (\c ->
                ( chartableId
                , { name = c.name
                  , colour = toColour userData c
                  , dataPoints = toDataPoints userData c
                  , visible = visible
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateGraph fn m =
            { m | graph = fn m.graph }

        setUserData userData_ m =
            { m | userData = userData_ }
    in
    case msg of
        ChartFillLinesChecked fl ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart model.chartId (LineChart.setFillLines fl)
            in
            ( model
                |> setUserData userData_
                |> (updateGraph <| \c -> { c | fillLines = fl })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartFullScreenClicked ->
            ( model, toggleElementFullScreen ("chart" ++ LineChartId.toString model.chartId) )

        ChartClicked ->
            ( model |> updateGraph (\c -> { c | selectedDataSet = Nothing, selectedDataPoint = Nothing }), Cmd.none )

        ChartZoomOutClicked ->
            ( model |> updateGraph (\c -> { c | xScale = c.xScale * 3 / 4 })
            , Task.map2 Tuple.pair
                (Dom.getViewportOf <| "chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                (Dom.getElement <| "chart" ++ LineChartId.toString model.chartId ++ "-svg")
                |> Task.attempt ViewportUpdated
            )

        ChartZoomInClicked ->
            ( model |> updateGraph (\c -> { c | xScale = c.xScale * 4 / 3 })
            , Task.map2 Tuple.pair
                (Dom.getViewportOf <| "chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                (Dom.getElement <| "chart" ++ LineChartId.toString model.chartId ++ "-svg")
                |> Task.attempt ViewportUpdated
            )

        ChartExpandValueClicked ->
            ( model |> (\c -> { c | expandedValue = not c.expandedValue })
            , Cmd.none
            )

        ChartExpandValueCloseClicked ->
            ( model
                |> (\c -> { c | expandedValue = False })
                |> (updateGraph <| Graph.selectDataSet Nothing)
            , Cmd.none
            )

        FullScreenChanged fullScreen ->
            ( { model | fullScreen = fullScreen }, Cmd.none )

        ViewportUpdated (Ok ( scrollable, svg )) ->
            ( model
                |> (\c -> { c | viewport = Just scrollable })
                |> (updateGraph <| \c -> { c | currentWidth = svg.element.width, minWidth = scrollable.viewport.width })
            , Cmd.none
            )

        GraphMsg (Graph.MouseDown ( x, y )) ->
            case model.viewport of
                Just { scene } ->
                    let
                        xPerc =
                            x / scene.width

                        yPerc =
                            y / scene.height
                    in
                    ( model |> (updateGraph <| Graph.selectNearestDataPoint ( xPerc, yPerc )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GraphMsg (Graph.MouseMove ( x, y )) ->
            case model.viewport of
                Just { scene } ->
                    let
                        xPerc =
                            x / scene.width

                        yPerc =
                            y / scene.height
                    in
                    ( model |> (updateGraph <| Graph.hoverNearestDataPoint ( xPerc, yPerc )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GraphMsg graphMsg ->
            ( model |> (updateGraph <| Graph.update graphMsg), Cmd.none )

        _ ->
            ( model, Cmd.none )


updateDataPoints : UserData -> ChartableId -> Model -> Model
updateDataPoints userData chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just chartable ->
            let
                graph =
                    model.graph
            in
            { model | graph = { graph | data = graph.data |> Listx.updateLookup chartableId (\d -> { d | dataPoints = toDataPoints userData chartable }) } }

        _ ->
            model


updateColour : UserData -> ChartableId -> Model -> Model
updateColour userData chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just chartable ->
            let
                graph =
                    model.graph
            in
            { model | graph = { graph | data = graph.data |> Listx.updateLookup chartableId (\d -> { d | colour = toColour userData chartable }) } }

        _ ->
            model


addDataSet : UserData -> ChartableId -> Model -> Model
addDataSet userData chartableId model =
    let
        graph =
            model.graph
    in
    case toDataSet userData chartableId True of
        Just ( _, dataSet ) ->
            { model | graph = { graph | data = graph.data |> Listx.insertLookup chartableId dataSet } }

        _ ->
            model


removeDataSet : ChartableId -> Model -> Model
removeDataSet chartableId model =
    let
        graph =
            model.graph
    in
    { model | graph = { graph | data = graph.data |> List.filter (\( cId, _ ) -> cId /= chartableId) } }


subscriptions : Model -> Sub Msg
subscriptions _ =
    fullScreenChanged FullScreenChanged


view : List ( ChartableId, String ) -> UserData -> Model -> Html Msg
view chartableOptions userData model =
    div
        [ id ("chart" ++ LineChartId.toString model.chartId)
        , class "mb-8 bg-white"
        , classList
            [ ( "p-8", model.fullScreen )
            ]
        ]
        [ div
            [ class "mr-4 my-0 flex scrollable-parent relative"
            , style "height" "300px"
            ]
            ([ viewJustYAxis "flex-grow-0 flex-shrink-0" model.graph
             , viewScrollableContainer ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable") [ Html.map GraphMsg <| viewLineGraph ("chart" ++ LineChartId.toString model.chartId ++ "-svg") "h-full" model.graph ]
             , div [ class "absolute right-2 top-6 flex flex-col" ]
                [ button
                    [ class "rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none text-opacity-70 hover:bg-opacity-100 hover:text-opacity-100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation ChartFullScreenClicked
                    ]
                    [ icon "w-5 h-5" <|
                        if model.fullScreen then
                            SolidCompress

                        else
                            SolidExpand
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none text-opacity-70 hover:bg-opacity-100 hover:text-opacity-100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation ChartZoomInClicked
                    ]
                    [ icon "w-5 h-5" SolidPlus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-50 cursor-default", model.graph.currentWidth <= model.graph.minWidth )
                        , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", model.graph.currentWidth > model.graph.minWidth )
                        ]
                    , Htmlx.onClickStopPropagation ChartZoomOutClicked
                    , disabled (model.graph.currentWidth <= model.graph.minWidth)
                    ]
                    [ icon "w-5 h-5" <| SolidMinus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none fill-icon text-opacity-70 hover:bg-opacity-100 focus:bg-opacity:100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation (ChartFillLinesChecked <| not model.graph.fillLines)
                    ]
                    [ fillIcon "w-5 h-5" model.graph.fillLines ]
                ]
             ]
                ++ (case model.graph.selectedDataSet |> Maybe.andThen (\id -> Listx.findBy Tuple.first id chartableOptions) of
                        Just ( id, name ) ->
                            [ div
                                [ class "absolute left-14 top-6 rounded bg-white bg-opacity-80 p-2 min-w-44 max-w-xs" ]
                                ([ button
                                    [ class "absolute right-2 top-2 text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                    , Htmlx.onClickStopPropagation ChartExpandValueCloseClicked
                                    ]
                                    [ icon "w-4 h-4" SolidTimes
                                    ]
                                 , h4 [ class "font-bold pr-5" ] [ text name ]
                                 ]
                                    ++ (let
                                            featuredDataPoint =
                                                case model.graph.selectedDataPoint of
                                                    Just p ->
                                                        Just p

                                                    _ ->
                                                        model.graph.hoveredDataPoint
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
                                                                    model.graph.data
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
                                                        , Htmlx.onClickPreventDefault ChartExpandValueClicked
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


viewScrollableContainer : String -> List (Html msg) -> Html msg
viewScrollableContainer containerId children =
    div [ class "relative flex-grow" ]
        [ node "scrollable-container"
            [ id containerId
            , class "absolute overflow-x-scroll top-0 left-0 right-0 bottom-0"
            ]
            children
        ]