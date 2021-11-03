module Chart.LineChart exposing (DataSetId(..), Model, Msg, addChartableDataSet, addTrackableDataSet, hoverDataSet, init, moveDataSetBack, moveDataSetForward, removeDataSet, selectDataSet, subscriptions, toggleDataSetSelected, toggleDataSetVisible, update, updateChartableData, updateDataSetColour, updateDataSetName, updateName, updateTrackableData, view, replaceTrackableWithChartableDataSet)

import Array
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
import UserData.LineChart as LineChart exposing (LineChart, LineChartData(..))
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..))
import UserData.TrackableId exposing (TrackableId)


type alias Model =
    { chartId : LineChartId
    , name : String
    , viewport : Maybe Dom.Viewport
    , expandedValue : Bool
    , userData : UserData
    , fullScreen : Bool
    , graph : Graph.Model DataSetId
    , data : List ( DataSetId, { name : String, inverted : Bool, data : List ( String, Dict Int Float, Float ) } )
    }


type DataSetId
    = ChartableId ChartableId
    | TrackableId TrackableId


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
    | GraphMsg (Graph.Msg DataSetId)


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
                chart.data
                    |> Array.toList
                    |> List.filterMap
                        (\( data, visible ) ->
                            case data of
                                Chartable chartableId ->
                                    chartableToDataSet userData chartableId visible

                                Trackable { id, multiplier, inverted } ->
                                    trackableToDataSet userData id multiplier inverted visible
                        )
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
      , data =
            chart.data
                |> Array.toList
                |> List.filterMap
                    (\( data, _ ) ->
                        case data of
                            Chartable chartableId ->
                                userData
                                    |> UserData.getChartable chartableId
                                    |> Maybe.map
                                        (\c ->
                                            ( ChartableId chartableId
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

                            Trackable { id, multiplier, inverted } ->
                                userData
                                    |> UserData.getTrackable id
                                    |> Maybe.map
                                        (\t ->
                                            ( TrackableId id
                                            , { name = t.question
                                              , inverted = inverted
                                              , data = [ ( t.question, Trackable.onlyFloatData t, multiplier ) ]
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
            |> Task.andThen (\_ -> Dom.getViewportOf elementId)
            |> Task.attempt ViewportUpdated
        ]
    )


chartableToDataSet : UserData -> ChartableId -> Bool -> Maybe ( DataSetId, Graph.DataSet )
chartableToDataSet userData chartableId visible =
    userData
        |> UserData.getChartable chartableId
        |> Maybe.map
            (\c ->
                ( ChartableId chartableId
                , { name = c.name
                  , colour = UserData.getChartableColour userData c
                  , dataPoints = UserData.getChartableDataPoints userData c
                  , visible = visible
                  }
                )
            )


trackableToDataSet : UserData -> TrackableId -> Float -> Bool -> Bool -> Maybe ( DataSetId, Graph.DataSet )
trackableToDataSet userData id multiplier inverted visible =
    userData
        |> UserData.getTrackable id
        |> Maybe.map
            (\t ->
                ( TrackableId id
                , { name = t.question
                  , colour = t.colour
                  , dataPoints = UserData.getTrackableDataPoints multiplier inverted t
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


hoverDataSet : Maybe DataSetId -> Model -> Model
hoverDataSet id model =
    { model | graph = model.graph |> Graph.hoverDataSet id }


toggleDataSetVisible : DataSetId -> Model -> Model
toggleDataSetVisible id model =
    { model | graph = model.graph |> Graph.toggleDataSetVisible id }


toggleDataSetSelected : DataSetId -> Model -> Model
toggleDataSetSelected id model =
    { model | graph = model.graph |> Graph.toggleDataSetSelected id }


selectDataSet : Maybe DataSetId -> Model -> Model
selectDataSet id model =
    { model | graph = model.graph |> Graph.selectDataSet id }


updateDataSetName : DataSetId -> String -> Model -> Model
updateDataSetName id name model =
    let
        graph =
            model.graph
    in
    { model
        | graph = { graph | data = graph.data |> Listx.updateLookup id (\d -> { d | name = name }) }
        , data = model.data |> Listx.updateLookup id (\d -> { d | name = name })
    }


updateChartableData : UserData -> ChartableId -> Model -> Model
updateChartableData userData chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just chartable ->
            let
                graph =
                    model.graph
            in
            { model
                | graph = { graph | data = graph.data |> Listx.updateLookup (ChartableId chartableId) (\d -> { d | dataPoints = UserData.getChartableDataPoints userData chartable }) }
                , data =
                    model.data
                        |> Listx.updateLookup (ChartableId chartableId)
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


updateTrackableData : UserData -> TrackableId -> Maybe Float -> Maybe Bool -> Model -> Model
updateTrackableData userData trackableId multiplierM invertedM model =
    case userData |> UserData.getTrackable trackableId of
        Just trackable ->
            let
                graph =
                    model.graph

                trackableDataM =
                    model.data |> Listx.lookup (TrackableId trackableId)

                trackableListDataM =
                    trackableDataM |> Maybe.andThen (.data >> List.head)
            in
            case ( trackableDataM, trackableListDataM ) of
                ( Just { inverted }, Just ( _, _, multiplier ) ) ->
                    { model
                        | graph = { graph | data = graph.data |> Listx.updateLookup (TrackableId trackableId) (\d -> { d | dataPoints = UserData.getTrackableDataPoints (Maybe.withDefault multiplier multiplierM) (Maybe.withDefault inverted invertedM) trackable }) }
                        , data =
                            model.data
                                |> Listx.updateLookup (TrackableId trackableId)
                                    (\d ->
                                        { d
                                            | data = [ ( trackable.question, Trackable.onlyFloatData trackable, Maybe.withDefault multiplier multiplierM ) ]
                                            , inverted = Maybe.withDefault inverted invertedM
                                        }
                                    )
                    }

                _ ->
                    model

        _ ->
            model


updateDataSetColour : UserData -> DataSetId -> Model -> Model
updateDataSetColour userData dataSetId model =
    case dataSetId of
        ChartableId chartableId ->
            case userData |> UserData.getChartable chartableId of
                Just chartable ->
                    let
                        graph =
                            model.graph
                    in
                    { model
                        | graph = { graph | data = graph.data |> Listx.updateLookup dataSetId (\d -> { d | colour = UserData.getChartableColour userData chartable }) }
                    }

                _ ->
                    model

        _ ->
            model


addChartableDataSet : UserData -> ChartableId -> Model -> ( Model, Cmd Msg )
addChartableDataSet userData chartableId model =
    let
        graph =
            model.graph
    in
    case ( chartableToDataSet userData chartableId True, userData |> UserData.getChartable chartableId ) of
        ( Just ( _, dataSet ), Just chartable ) ->
            ( { model
                | graph = { graph | data = graph.data |> Listx.insertLookup (ChartableId chartableId) dataSet }
                , data =
                    model.data
                        |> Listx.insertLookup (ChartableId chartableId)
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
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        _ ->
            ( model, Cmd.none )

replaceTrackableWithChartableDataSet : UserData -> Int -> ChartableId -> Model -> ( Model, Cmd Msg )
replaceTrackableWithChartableDataSet userData i chartableId model =
    let
        graph =
            model.graph
    in
    case ( chartableToDataSet userData chartableId True, userData |> UserData.getChartable chartableId ) of
        ( Just ( _, dataSet ), Just chartable ) ->
            ( { model
                | graph = { graph | data = List.take i graph.data ++ (ChartableId chartableId, dataSet) :: List.drop (i + 1) graph.data }
                , data =
                    List.take i model.data ++ (ChartableId chartableId,
                            { name = chartable.name
                            , inverted = chartable.inverted
                            , data =
                                chartable.sum
                                    |> List.filterMap
                                        (\( tId, m ) ->
                                            userData |> UserData.getTrackable tId |> Maybe.map (\t -> ( t.question, Trackable.onlyFloatData t, m ))
                                        )
                            }) :: List.drop (i + 1) model.data
              }
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        _ ->
            ( model, Cmd.none )


addTrackableDataSet : UserData -> TrackableId -> Model -> ( Model, Cmd Msg )
addTrackableDataSet userData trackableId model =
    let
        graph =
            model.graph
    in
    case ( trackableToDataSet userData trackableId 1 False True, userData |> UserData.getTrackable trackableId ) of
        ( Just ( _, dataSet ), Just trackable ) ->
            ( { model
                | graph = { graph | data = graph.data |> Listx.insertLookup (TrackableId trackableId) dataSet }
                , data =
                    model.data
                        |> Listx.insertLookup (TrackableId trackableId)
                            { name = trackable.question
                            , inverted = False
                            , data = [ ( trackable.question, Trackable.onlyFloatData trackable, 1 ) ]
                            }
              }
            , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        _ ->
            ( model, Cmd.none )


removeDataSet : DataSetId -> Model -> ( Model, Cmd Msg )
removeDataSet dataSetId model =
    let
        graph =
            model.graph
    in
    ( { model
        | graph = { graph | data = graph.data |> List.filter (\( cId, _ ) -> cId /= dataSetId) }
        , data = model.data |> List.filter (\( cId, _ ) -> cId /= dataSetId)
      }
    , Dom.getViewportOf ("chart" ++ LineChartId.toString model.chartId ++ "-scrollable")
        |> Task.attempt ViewportUpdated
    )


moveDataSetBack : DataSetId -> Model -> Model
moveDataSetBack dataSetId model =
    let
        graph =
            model.graph
    in
    { model | graph = { graph | data = graph.data |> Listx.moveHeadwardsBy Tuple.first dataSetId } }


moveDataSetForward : DataSetId -> Model -> Model
moveDataSetForward dataSetId model =
    let
        graph =
            model.graph
    in
    { model | graph = { graph | data = graph.data |> Listx.moveTailwardsBy Tuple.first dataSetId } }


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
                , if List.isEmpty model.data then
                    div [ class "absolute inset-0 flex justify-center items-center" ] [ span [ class "mb-6" ] [ text "No data added yet" ] ]

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
                        [ ( "text-opacity-30 cursor-default", List.isEmpty model.data )
                        , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", not (List.isEmpty model.data) )
                        ]
                    , Htmlx.onClickStopPropagation ChartZoomInClicked
                    , disabled (List.isEmpty model.data)
                    ]
                    [ icon "w-5 h-5" SolidPlus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", List.isEmpty model.data || model.graph.currentWidth > 0 && model.graph.currentWidth <= model.graph.minWidth )
                        , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", not (List.isEmpty model.data) && model.graph.currentWidth > model.graph.minWidth )
                        ]
                    , Htmlx.onClickStopPropagation ChartZoomOutClicked
                    , disabled (List.isEmpty model.data || model.graph.currentWidth > 0 && model.graph.currentWidth <= model.graph.minWidth)
                    ]
                    [ icon "w-5 h-5" SolidMinus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none fill-icon"
                    , classList
                        [ ( "disabled cursor-default", List.isEmpty model.data )
                        ]
                    , Htmlx.onClickStopPropagation (ChartFillLinesChecked <| not model.graph.fillLines)
                    , disabled (List.isEmpty model.data)
                    ]
                    [ fillIcon "w-5 h-5" model.graph.fillLines ]
                ]
             ]
                ++ (case model.graph.selectedDataSet |> Maybe.andThen (\id -> Listx.findBy Tuple.first id model.data) of
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
                                                [ p [ class "mt-2 text-sm" ]
                                                    [ a
                                                        [ href <| "/day/" ++ (Date.format "y/M/d" <| Date.fromRataDie date)
                                                        , class "text-sm text-blue-600 hover:text-blue-800 underline"
                                                        ]
                                                        [ text <| Date.format "EEE d MMM y" <| Date.fromRataDie date ]
                                                    ]
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
                                                                                    , span [ class "font-bold" ] [ text <| String.fromFloat max ]
                                                                                    , text <| " (max value)"
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
