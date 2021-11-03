module Chart.LineChart exposing (DataSetId(..), Model, Msg, addChartableDataSet, addTrackableDataSet, hoverDataSet, init, moveDataSetBack, moveDataSetForward, removeDataSet, replaceDataSetWithChartable, replaceDataSetWithTrackable, selectDataSet, subscriptions, toggleDataSetSelected, toggleDataSetVisible, update, updateChartableData, updateDataSetColour, updateDataSetName, updateName, updateTrackableData, view)

import Array exposing (Array)
import Arrayx
import Browser.Dom as Dom
import Browser.Events as E
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Htmlx
import Maybe exposing (Maybe)
import Ports exposing (fullScreenChanged, toggleElementFullScreen)
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), fillIcon, icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as C exposing (Chartable)
import UserData.ChartableId exposing (ChartableId)
import UserData.LineChart as LC exposing (DataSet(..), LineChart(..))
import UserData.LineChartId as LCId exposing (LineChartId)
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId exposing (TrackableId)


type alias Model =
    { chartId : LineChartId
    , name : String
    , viewport : Maybe Dom.Viewport
    , expandedValue : Bool
    , userData : UserData
    , fullScreen : Bool
    , graph : Graph.Model
    , data : Array DataSet
    }


type alias DataSet =
    { name : String
    , inverted : Bool
    , data : List ( String, Dict Int Float, Float )
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
    | GraphMsg Graph.Msg


init : Date -> UserData -> LineChartId -> LineChart -> ( Model, Cmd Msg )
init today userData chartId chart =
    ( { chartId = chartId
      , name = LC.name chart
      , viewport = Nothing
      , expandedValue = False
      , userData = userData
      , fullScreen = False
      , graph =
            { today = today
            , fillLines = LC.fillLines chart
            , data =
                LC.dataSets chart
                    |> Array.map
                        (\( data, visible ) ->
                            case data of
                                LC.Chartable { chartableId, chartable } ->
                                    chartableToDataSet chartable visible

                                LC.Trackable { trackableId, trackable, multiplier, isInverted } ->
                                    trackableToDataSet trackable multiplier isInverted visible
                        )
            , selectedDataSet = Nothing
            , leavingDataSet = Nothing
            , hoveredDataSet = Nothing
            , selectedDataPoint = Nothing
            , hoveredDataPoint = Nothing
            , xScale = 1
            , minWidth = 0
            , maxWidth = 0
            , currentWidth = 0
            , height = 0
            }
      , data =
            LC.dataSets chart
                |> Array.map
                    (\( data, _ ) ->
                        case data of
                            LC.Chartable { chartableId, chartable } ->
                                { name = C.name chartable
                                , inverted = C.isInverted chartable
                                , data =
                                    C.sum chartable
                                        |> List.map
                                            (\( _, ( t, m ) ) ->
                                                ( T.question t, T.onlyFloatData t, m )
                                            )
                                }

                            LC.Trackable { trackableId, trackable, multiplier, isInverted } ->
                                { name = T.question trackable
                                , inverted = isInverted
                                , data = [ ( T.question trackable, T.onlyFloatData trackable, multiplier ) ]
                                }
                    )
      }
    , Cmd.batch
        [ let
            elementId =
                "chart" ++ LCId.toString chartId ++ "-scrollable"
          in
          Dom.getViewportOf elementId
            |> Task.andThen (\info -> Dom.setViewportOf elementId info.scene.width 0)
            |> Task.andThen (\_ -> Dom.getViewportOf elementId)
            |> Task.attempt ViewportUpdated
        ]
    )


chartableToDataSet : Chartable -> Bool -> Graph.DataSet
chartableToDataSet c visible =
    { name = C.name c
    , colour = C.colour c
    , dataPoints = C.dataPoints c
    , visible = visible
    }


trackableToDataSet : Trackable -> Float -> Bool -> Bool -> Graph.DataSet
trackableToDataSet t multiplier inverted visible =
    { name = T.question t
    , colour = T.colour t
    , dataPoints = T.getDataPoints multiplier inverted t
    , visible = visible
    }


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
                    model.userData |> UserData.updateLineChart model.chartId (LC.setFillLines fl)
            in
            ( model
                |> setUserData userData_
                |> (updateGraph <| \c -> { c | fillLines = fl })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartFullScreenClicked ->
            ( model, toggleElementFullScreen ("chart" ++ LCId.toString model.chartId) )

        ChartClicked ->
            ( model |> updateGraph (\c -> { c | selectedDataSet = Nothing, selectedDataPoint = Nothing }), Cmd.none )

        ChartZoomOutClicked ->
            ( model
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ChartZoomOutRequested
            )

        ChartZoomInClicked ->
            ( model
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ChartZoomInRequested
            )

        ChartZoomOutRequested (Ok scrollable) ->
            ( model |> updateGraph (\c -> { c | xScale = c.xScale * 3 / 4 })
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.andThen (\v -> Dom.setViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable") (v.scene.width * ((scrollable.viewport.x + scrollable.viewport.width / 2) / scrollable.scene.width) - (v.viewport.width / 2)) 0)
                |> Task.andThen (\_ -> Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable"))
                |> Task.attempt ViewportUpdated
            )

        ChartZoomInRequested (Ok scrollable) ->
            ( model |> updateGraph (\c -> { c | xScale = c.xScale * 4 / 3 })
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.andThen (\v -> Dom.setViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable") (v.scene.width * ((scrollable.viewport.x + scrollable.viewport.width / 2) / scrollable.scene.width) - (v.viewport.width / 2)) 0)
                |> Task.andThen (\_ -> Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable"))
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
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        ViewportUpdated (Ok scrollable) ->
            ( model
                |> (\c -> { c | viewport = Just scrollable })
            , Dom.getElement ("chart" ++ LCId.toString model.chartId ++ "-svg")
                |> Task.attempt ElementUpdated
            )

        ElementUpdated (Ok svg) ->
            case model.viewport of
                Just viewport ->
                    ( model
                        |> (updateGraph <| \c -> { c | currentWidth = svg.element.width, minWidth = viewport.viewport.width, maxWidth = viewport.scene.width, height = svg.element.height })
                    , if model.graph.currentWidth /= svg.element.width then
                        Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                            |> Task.attempt ViewportUpdated

                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GraphMsg graphMsg ->
            let
                ( graph_, c ) =
                    Graph.update graphMsg model.graph
            in
            ( { model | graph = graph_ }, Cmd.map GraphMsg c )

        _ ->
            ( model, Cmd.none )


updateName : String -> Model -> ( Model, Cmd Msg )
updateName name model =
    ( { model | name = name }, Cmd.none )


hoverDataSet : Maybe Int -> Model -> ( Model, Cmd Msg )
hoverDataSet i model =
    ( { model | graph = model.graph |> Graph.hoverDataSet i }, Cmd.none )


toggleDataSetVisible : Int -> Model -> ( Model, Cmd Msg )
toggleDataSetVisible i model =
    ( { model | graph = model.graph |> Graph.toggleDataSetVisible i }, Cmd.none )


toggleDataSetSelected : Int -> Model -> ( Model, Cmd Msg )
toggleDataSetSelected i model =
    ( { model | graph = model.graph |> Graph.toggleDataSetSelected i }, Cmd.none )


selectDataSet : Maybe Int -> Model -> ( Model, Cmd Msg )
selectDataSet i model =
    ( { model | graph = model.graph |> Graph.selectDataSet i }, Cmd.none )


updateDataSetName : Int -> String -> Model -> ( Model, Cmd Msg )
updateDataSetName i name model =
    let
        graph =
            model.graph
    in
    ( { model
        | graph = { graph | data = graph.data |> Arrayx.update i (\d -> { d | name = name }) }
        , data = model.data |> Arrayx.update i (\d -> { d | name = name })
      }
    , Cmd.none
    )


updateChartableData : Int -> UserData -> ChartableId -> Model -> ( Model, Cmd Msg )
updateChartableData i userData chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just ch ->
            let
                graph =
                    model.graph
            in
            ( { model
                | graph = { graph | data = graph.data |> Arrayx.update i (\d -> { d | dataPoints = C.dataPoints ch }) }
                , data =
                    model.data
                        |> Arrayx.update i
                            (\c ->
                                { c
                                    | inverted = C.isInverted ch
                                    , data =
                                        C.sum ch
                                            |> List.map
                                                (\( _, ( t, m ) ) ->
                                                    ( T.question t, T.onlyFloatData t, m )
                                                )
                                }
                            )
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateTrackableData : Int -> UserData -> TrackableId -> Maybe Float -> Maybe Bool -> Model -> ( Model, Cmd Msg )
updateTrackableData i userData trackableId multiplierM invertedM model =
    case userData |> UserData.getTrackable trackableId of
        Just trackable ->
            let
                graph =
                    model.graph

                trackableDataM =
                    model.data |> Array.get i

                trackableListDataM =
                    trackableDataM |> Maybe.andThen (.data >> List.head)
            in
            case ( trackableDataM, trackableListDataM ) of
                ( Just { inverted }, Just ( _, _, multiplier ) ) ->
                    ( { model
                        | graph = { graph | data = graph.data |> Arrayx.update i (\d -> { d | dataPoints = T.getDataPoints (Maybe.withDefault multiplier multiplierM) (Maybe.withDefault inverted invertedM) trackable }) }
                        , data =
                            model.data
                                |> Arrayx.update i
                                    (\d ->
                                        { d
                                            | data = [ ( T.question trackable, T.onlyFloatData trackable, Maybe.withDefault multiplier multiplierM ) ]
                                            , inverted = Maybe.withDefault inverted invertedM
                                        }
                                    )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateDataSetColour : Int -> UserData -> DataSetId -> Model -> ( Model, Cmd Msg )
updateDataSetColour i userData dataSetId model =
    case dataSetId of
        ChartableId chartableId ->
            case userData |> UserData.getChartable chartableId of
                Just c ->
                    let
                        graph =
                            model.graph
                    in
                    ( { model
                        | graph = { graph | data = graph.data |> Arrayx.update i (\d -> { d | colour = C.colour c }) }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


addChartableDataSet : UserData -> ChartableId -> Model -> ( Model, Cmd Msg )
addChartableDataSet userData chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just c ->
            let
                dataSet =
                    chartableToDataSet c True

                graph =
                    model.graph
            in
            ( { model
                | graph = { graph | data = graph.data |> Array.push dataSet }
                , data =
                    model.data
                        |> Array.push
                            { name = C.name c
                            , inverted = C.isInverted c
                            , data =
                                C.sum c
                                    |> List.map
                                        (\( _, ( t, m ) ) ->
                                            ( T.question t, T.onlyFloatData t, m )
                                        )
                            }
              }
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        _ ->
            ( model, Cmd.none )


replaceDataSetWithChartable : UserData -> Int -> ChartableId -> Model -> ( Model, Cmd Msg )
replaceDataSetWithChartable userData i chartableId model =
    case userData |> UserData.getChartable chartableId of
        Just c ->
            let
                graph =
                    model.graph

                dataSet =
                    chartableToDataSet c True
            in
            ( { model
                | graph = { graph | data = graph.data |> Array.set i dataSet }
                , data =
                    model.data
                        |> Array.set i
                            { name = C.name c
                            , inverted = C.isInverted c
                            , data =
                                C.sum c
                                    |> List.map
                                        (\( _, ( t, m ) ) ->
                                            ( T.question t, T.onlyFloatData t, m )
                                        )
                            }
              }
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        _ ->
            ( model, Cmd.none )


replaceDataSetWithTrackable : UserData -> Int -> TrackableId -> Float -> Bool -> Bool -> Model -> ( Model, Cmd Msg )
replaceDataSetWithTrackable userData i trackableId multiplier inverted visible model =
    case userData |> UserData.getTrackable trackableId of
        Just trackable ->
            let
                dataSet =
                    trackableToDataSet trackable multiplier inverted visible

                graph =
                    model.graph
            in
            ( { model
                | graph = { graph | data = graph.data |> Array.set i dataSet }
                , data =
                    model.data
                        |> Array.set i
                            { name = T.question trackable
                            , inverted = inverted
                            , data = [ ( T.question trackable, T.onlyFloatData trackable, multiplier ) ]
                            }
              }
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        _ ->
            ( model, Cmd.none )


addTrackableDataSet : UserData -> TrackableId -> Model -> ( Model, Cmd Msg )
addTrackableDataSet userData trackableId model =
    case userData |> UserData.getTrackable trackableId of
        Just trackable ->
            let
                dataSet =
                    trackableToDataSet trackable 1 False True

                graph =
                    model.graph
            in
            ( { model
                | graph = { graph | data = graph.data |> Array.push dataSet }
                , data =
                    model.data
                        |> Array.push
                            { name = T.question trackable
                            , inverted = False
                            , data = [ ( T.question trackable, T.onlyFloatData trackable, 1 ) ]
                            }
              }
            , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                |> Task.attempt ViewportUpdated
            )

        _ ->
            ( model, Cmd.none )


removeDataSet : Int -> Model -> ( Model, Cmd Msg )
removeDataSet i model =
    let
        graph =
            model.graph
    in
    ( { model
        | graph = { graph | data = graph.data |> Arrayx.delete i }
        , data = model.data |> Arrayx.delete i
      }
    , Dom.getViewportOf ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
        |> Task.attempt ViewportUpdated
    )


moveDataSetBack : Int -> Model -> ( Model, Cmd Msg )
moveDataSetBack i model =
    let
        graph =
            model.graph
    in
    ( { model | graph = { graph | data = graph.data |> Arrayx.swap i (i - 1) } }, Cmd.none )


moveDataSetForward : Int -> Model -> ( Model, Cmd Msg )
moveDataSetForward i model =
    let
        graph =
            model.graph
    in
    ( { model | graph = { graph | data = graph.data |> Arrayx.swap i (i + 1) } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fullScreenChanged FullScreenChanged
        , E.onResize (\_ _ -> WindowResized)
        ]


view : Model -> Html Msg
view model =
    div
        [ id ("chart" ++ LCId.toString model.chartId)
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
                    [ id ("chart" ++ LCId.toString model.chartId ++ "-scrollable")
                    , class "absolute overflow-x-scroll top-0 left-0 right-0 bottom-0"
                    ]
                    [ Html.map GraphMsg <| viewLineGraph ("chart" ++ LCId.toString model.chartId ++ "-svg") "h-full" model.graph ]
                , if Array.isEmpty model.data then
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
                        [ ( "text-opacity-30 cursor-default", Array.isEmpty model.data )
                        , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", not (Array.isEmpty model.data) )
                        ]
                    , Htmlx.onClickStopPropagation ChartZoomInClicked
                    , disabled (Array.isEmpty model.data)
                    ]
                    [ icon "w-5 h-5" SolidPlus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", Array.isEmpty model.data || model.graph.currentWidth > 0 && model.graph.currentWidth <= model.graph.minWidth )
                        , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", not (Array.isEmpty model.data) && model.graph.currentWidth > model.graph.minWidth )
                        ]
                    , Htmlx.onClickStopPropagation ChartZoomOutClicked
                    , disabled (Array.isEmpty model.data || model.graph.currentWidth > 0 && model.graph.currentWidth <= model.graph.minWidth)
                    ]
                    [ icon "w-5 h-5" SolidMinus
                    ]
                , button
                    [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none fill-icon"
                    , classList
                        [ ( "disabled cursor-default", Array.isEmpty model.data )
                        ]
                    , Htmlx.onClickStopPropagation (ChartFillLinesChecked <| not model.graph.fillLines)
                    , disabled (Array.isEmpty model.data)
                    ]
                    [ fillIcon "w-5 h-5" model.graph.fillLines ]
                ]
             ]
                ++ (case ( model.graph.selectedDataSet, model.graph.selectedDataSet |> Maybe.andThen (\i -> Array.get i model.data) ) of
                        ( Just i, Just { name, inverted, data } ) ->
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
                                                                        |> Array.get i
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
                                                                    (\di ( question, d, multiplier ) ->
                                                                        let
                                                                            value =
                                                                                Maybe.withDefault 0 <| Dict.get date <| d
                                                                        in
                                                                        tr []
                                                                            [ td [ class "align-baseline" ]
                                                                                [ icon "w-2 h-2" <|
                                                                                    if di == 0 then
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
