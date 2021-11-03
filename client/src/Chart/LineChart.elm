module Chart.LineChart exposing (Model, Msg, addDataSet, hoverDataSet, init, moveDataSetBack, moveDataSetForward, removeDataSet, selectDataSet, subscriptions, toggleDataSetSelected, toggleDataSetVisible, update, updateDataSetColour, updateDataSetData, updateDataSetName, updateName, view)

import Browser.Dom as Dom
import Browser.Events as E
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Htmlx
import Listx
import Maybe exposing (Maybe)
import Ports exposing (fullScreenChanged, toggleElementFullScreen)
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), fillIcon, icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.ChartableId exposing (ChartableId)
import UserData.LineChart as LineChart exposing (LineChart)
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..))


type alias Model =
    { chartId : LineChartId
    , name : String
    , viewport : Maybe Dom.Viewport
    , expandedValue : Bool
    , userData : UserData
    , fullScreen : Bool
    , graph : Graph.Model ChartableId
    , chartables : List ( ChartableId, { name : String, inverted : Bool, data : List ( String, Dict Int Float, Float ) } )
    }


type Msg
    = NoOp
    | ChartFillLinesChecked Bool
    | ChartFullScreenClicked
    | ChartZoomOutClicked
    | ChartZoomInClicked
    | ChartZoomOutRequested (Result Dom.Error Dom.Viewport)
    | ChartZoomInRequested (Result Dom.Error Dom.Viewport)
    | ChartClicked
    | ChartExpandValueClicked
    | ChartExpandValueCloseClicked
    | FullScreenChanged Bool
    | ViewportUpdated (Result Dom.Error Dom.Viewport)
    | ElementUpdated (Result Dom.Error Dom.Element)
    | WindowResized
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
            , height = 0
            }
      , chartables =
            chart.chartables
                |> List.filterMap
                    (\( chartableId, _ ) ->
                        userData
                            |> UserData.getChartable chartableId
                            |> Maybe.map
                                (\c ->
                                    ( chartableId
                                    , { name = c.name
                                      , inverted = c.inverted
                                      , data =
                                            c.sum
                                                |> List.filterMap
                                                    (\( tId, m ) ->
                                                        userData |> UserData.getTrackable tId |> Maybe.map (\t -> ( t.question, Trackable.onlyFloatData t, m ))
                                                    )
                                      }
                                    )
                                )
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
        , Dom.getViewportOf ("chart" ++ LineChartId.toString chartId ++ "-scrollable")
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
                  , colour = UserData.getChartableColour userData c
                  , dataPoints = UserData.getChartableDataPoints userData c
                  , visible = visible
                  }
                )
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
            ( model
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ChartZoomOutRequested
            )

        ChartZoomInClicked ->
            ( model
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ChartZoomInRequested
            )

        ChartZoomOutRequested (Ok scrollable) ->
            ( model |> updateGraph (\c -> { c | xScale = c.xScale * 3 / 4 })
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.andThen (\v -> Dom.setViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable") (v.scene.width * ((scrollable.viewport.x + scrollable.viewport.width / 2) / scrollable.scene.width) - (v.viewport.width / 2)) 0)
                |> Task.andThen (\_ -> Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable"))
                |> Task.attempt ViewportUpdated
            )

        ChartZoomInRequested (Ok scrollable) ->
            ( model |> updateGraph (\c -> { c | xScale = c.xScale * 4 / 3 })
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.andThen (\v -> Dom.setViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable") (v.scene.width * ((scrollable.viewport.x + scrollable.viewport.width / 2) / scrollable.scene.width) - (v.viewport.width / 2)) 0)
                |> Task.andThen (\_ -> Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable"))
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

        WindowResized ->
            ( model
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        ViewportUpdated (Ok scrollable) ->
            ( model
                |> (\c -> { c | viewport = Just scrollable })
            , Dom.getElement ("chart" ++ LineChartId.toString model.chartId ++ "-svg")
                |> Task.attempt ElementUpdated
            )

        ElementUpdated (Ok svg) ->
            case model.viewport of
                Just viewport ->
                    ( model
                        |> (updateGraph <| \c -> { c | currentWidth = svg.element.width, minWidth = viewport.viewport.width, height = svg.element.height })
                    , if model.graph.currentWidth /= svg.element.width then
                        Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                            |> Task.attempt ViewportUpdated

                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GraphMsg graphMsg ->
            case graphMsg of
                Graph.MouseDown ( x, y ) ->
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

                Graph.MouseMove ( x, y ) ->
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

                Graph.DataLineHovered id ->
                    ( model |> (updateGraph <| Graph.hoverDataSet id), Cmd.none )

                Graph.DataLineClicked id ->
                    ( model |> (updateGraph <| Graph.toggleDataSetSelected id), Cmd.none )

        _ ->
            ( model, Cmd.none )


updateName : String -> Model -> Model
updateName name model =
    { model | name = name }


hoverDataSet : Maybe ChartableId -> Model -> Model
hoverDataSet id model =
    { model | graph = model.graph |> Graph.hoverDataSet id }


toggleDataSetVisible : ChartableId -> Model -> Model
toggleDataSetVisible id model =
    { model | graph = model.graph |> Graph.toggleDataSetVisible id }


toggleDataSetSelected : ChartableId -> Model -> Model
toggleDataSetSelected id model =
    { model | graph = model.graph |> Graph.toggleDataSetSelected id }


selectDataSet : Maybe ChartableId -> Model -> Model
selectDataSet id model =
    { model | graph = model.graph |> Graph.selectDataSet id }


updateDataSetName : ChartableId -> String -> Model -> Model
updateDataSetName id name model =
    let
        graph =
            model.graph
    in
    { model
        | graph = { graph | data = graph.data |> Listx.updateLookup id (\d -> { d | name = name }) }
        , chartables = model.chartables |> Listx.updateLookup id (\d -> { d | name = name })
    }


updateDataSetData : UserData -> ChartableId -> Model -> Model
updateDataSetData userData chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just chartable ->
            let
                graph =
                    model.graph
            in
            { model
                | graph = { graph | data = graph.data |> Listx.updateLookup chartableId (\d -> { d | dataPoints = UserData.getChartableDataPoints userData chartable }) }
                , chartables =
                    model.chartables
                        |> Listx.updateLookup chartableId
                            (\c ->
                                { c
                                    | inverted = chartable.inverted
                                    , data =
                                        chartable.sum
                                            |> List.filterMap
                                                (\( tId, m ) ->
                                                    userData |> UserData.getTrackable tId |> Maybe.map (\t -> ( t.question, Trackable.onlyFloatData t, m ))
                                                )
                                }
                            )
            }

        _ ->
            model


updateDataSetColour : UserData -> ChartableId -> Model -> Model
updateDataSetColour userData chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just chartable ->
            let
                graph =
                    model.graph
            in
            { model
                | graph = { graph | data = graph.data |> Listx.updateLookup chartableId (\d -> { d | colour = UserData.getChartableColour userData chartable }) }
            }

        _ ->
            model


addDataSet : UserData -> ChartableId -> Model -> Model
addDataSet userData chartableId model =
    let
        graph =
            model.graph
    in
    case ( toDataSet userData chartableId True, userData |> UserData.getChartable chartableId ) of
        ( Just ( _, dataSet ), Just chartable ) ->
            { model
                | graph = { graph | data = graph.data |> Listx.insertLookup chartableId dataSet }
                , chartables =
                    model.chartables
                        |> Listx.insertLookup chartableId
                            { name = chartable.name
                            , inverted = chartable.inverted
                            , data =
                                chartable.sum
                                    |> List.filterMap
                                        (\( tId, m ) ->
                                            userData |> UserData.getTrackable tId |> Maybe.map (\t -> ( t.question, Trackable.onlyFloatData t, m ))
                                        )
                            }
            }

        _ ->
            model


removeDataSet : ChartableId -> Model -> Model
removeDataSet chartableId model =
    let
        graph =
            model.graph
    in
    { model
        | graph = { graph | data = graph.data |> List.filter (\( cId, _ ) -> cId /= chartableId) }
        , chartables = model.chartables |> List.filter (\( cId, _ ) -> cId /= chartableId)
    }


moveDataSetBack : ChartableId -> Model -> Model
moveDataSetBack chartableId model =
    let
        graph =
            model.graph
    in
    { model | graph = { graph | data = graph.data |> Listx.moveHeadwardsBy Tuple.first chartableId } }


moveDataSetForward : ChartableId -> Model -> Model
moveDataSetForward chartableId model =
    let
        graph =
            model.graph
    in
    { model | graph = { graph | data = graph.data |> Listx.moveTailwardsBy Tuple.first chartableId } }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fullScreenChanged FullScreenChanged
        , E.onResize (\_ _ -> WindowResized)
        ]


view : Model -> Html Msg
view model =
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
             , div [ class "relative flex-grow" ]
                [ node "scrollable-container"
                    [ id ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                    , class "absolute overflow-x-scroll top-0 left-0 right-0 bottom-0"
                    ]
                    [ Html.map GraphMsg <| viewLineGraph ("chart" ++ LineChartId.toString model.chartId ++ "-svg") "h-full" model.graph ]
                , if List.isEmpty model.chartables then
                    div [ class "absolute inset-0 flex justify-center items-center" ] [ span [ class "mb-6" ] [ text "No data to display" ] ]

                  else
                    div [] []
                ]
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
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", List.isEmpty model.chartables )
                        , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", not (List.isEmpty model.chartables) )
                        ]
                    , Htmlx.onClickStopPropagation ChartZoomInClicked
                    , disabled (List.isEmpty model.chartables)
                    ]
                    [ icon "w-5 h-5" SolidPlus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", List.isEmpty model.chartables || model.graph.currentWidth > 0 && model.graph.currentWidth <= model.graph.minWidth )
                        , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", not (List.isEmpty model.chartables) && model.graph.currentWidth > model.graph.minWidth )
                        ]
                    , Htmlx.onClickStopPropagation ChartZoomOutClicked
                    , disabled (List.isEmpty model.chartables || model.graph.currentWidth > 0 && model.graph.currentWidth <= model.graph.minWidth)
                    ]
                    [ icon "w-5 h-5" SolidMinus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none fill-icon"
                    , classList
                        [ ( "disabled cursor-default", List.isEmpty model.chartables )
                        ]
                    , Htmlx.onClickStopPropagation (ChartFillLinesChecked <| not model.graph.fillLines)
                    , disabled (List.isEmpty model.chartables)
                    ]
                    [ fillIcon "w-5 h-5" model.graph.fillLines ]
                ]
             ]
                ++ (case model.graph.selectedDataSet |> Maybe.andThen (\id -> Listx.findBy Tuple.first id model.chartables) of
                        Just ( id, { name, inverted, data } ) ->
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
                                                    total =
                                                        List.sum <| List.map (\( _, d, m ) -> (Maybe.withDefault 0 <| Dict.get date <| d) * m) <| data

                                                    invertedTotal =
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
                                                        , span [ class "ml-1 font-bold" ] [ text <| String.fromFloat <| (toFloat <| round <| total * 100) / 100 ]
                                                        ]
                                                            ++ (case invertedTotal of
                                                                    Just ( _, t ) ->
                                                                        [ span [ class "ml-1" ] [ text "(inverted " ]
                                                                        , span [ class "font-bold" ] [ text <| String.fromFloat <| (toFloat <| round <| t * 100) / 100 ]
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
                                                                    (\i ( question, d, multiplier ) ->
                                                                        let
                                                                            value =
                                                                                Maybe.withDefault 0 <| Dict.get date <| d
                                                                        in
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
