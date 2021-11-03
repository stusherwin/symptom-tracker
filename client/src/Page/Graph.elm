module Page.Graph exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Chart exposing (Chart(..), ChartId, LineChartData)
import Chartable exposing (ChartableDict, ChartableId)
import Date exposing (Date, Unit(..))
import Dict
import Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, onMouseEnter, onMouseLeave, preventDefaultOn, stopPropagationOn)
import Icon exposing (IconType(..), icon)
import IdDict exposing (IdDict)
import Json.Decode as Decode
import Maybe exposing (Maybe)
import Task
import Time exposing (Month(..))
import Trackable exposing (TrackableData(..), TrackableDict)
import UserData exposing (UserData)


type alias Model =
    { today : Date
    , charts : IdDict ChartId ( Chart, Graph.Model ChartableId )
    , chartables : ChartableDict
    , trackables : TrackableDict
    }


init : Date -> UserData -> ( Model, Cmd Msg )
init today userData =
    let
        chartables =
            UserData.chartables userData

        trackables =
            UserData.trackables userData
    in
    ( { today = today
      , charts = UserData.charts userData |> IdDict.map (\_ c -> ( c, initGraph today c chartables trackables ))
      , chartables = chartables
      , trackables = trackables
      }
    , Dom.getViewportOf "chart"
        |> Task.andThen (\info -> Dom.setViewportOf "chart" info.scene.width 0)
        |> Task.attempt (always NoOp)
    )


initGraph : Date -> Chart -> ChartableDict -> TrackableDict -> Graph.Model ChartableId
initGraph today chart chartables trackables =
    case chart of
        LineChart c ->
            Graph.init today
                c.fillLines
                c.showPoints
                (c.chartableOrder
                    |> List.filterMap
                        (\id ->
                            IdDict.get id chartables
                                |> Maybe.map (\cb -> ( id, cb ))
                        )
                    |> Chartable.fromList
                    |> IdDict.map
                        (\_ cb ->
                            { name = cb.name
                            , colour = cb.colour
                            , dataPoints =
                                let
                                    dataPoints =
                                        cb.sum
                                            |> List.filterMap
                                                (\( id, multiplier ) ->
                                                    IdDict.get id trackables
                                                        |> Maybe.map
                                                            (\t ->
                                                                Dict.map (\_ v -> v * multiplier) <| Trackable.onlyFloatData t
                                                            )
                                                )
                                            |> List.foldl
                                                (\d1 d2 ->
                                                    Dict.merge
                                                        Dict.insert
                                                        (\d v1 v2 -> Dict.insert d (v1 + v2))
                                                        Dict.insert
                                                        d1
                                                        d2
                                                        Dict.empty
                                                )
                                                Dict.empty
                                in
                                if cb.inverted then
                                    let
                                        maxValue =
                                            List.maximum <| Dict.values dataPoints
                                    in
                                    case maxValue of
                                        Just max ->
                                            dataPoints |> Dict.map (\_ v -> max - v)

                                        _ ->
                                            dataPoints

                                else
                                    dataPoints
                            }
                        )
                )
                c.chartableOrder



-- UPDATE


type Msg
    = NoOp
    | FillLinesChecked ChartId Bool
    | ShowPointsChecked ChartId Bool
    | DataSetHovered ChartId (Maybe ChartableId)
    | DataSetClicked ChartId ChartableId
    | DataSetVisibleClicked ChartId ChartableId
    | DataSetBringForwardClicked ChartId ChartableId
    | DataSetPushBackClicked ChartId ChartableId
    | GraphMsg ChartId (Graph.Msg ChartableId)
    | UserDataChanged UserData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FillLinesChecked chartId fl ->
            ( { model | charts = model.charts |> IdDict.update chartId (Tuple.mapSecond <| Graph.setFillLines fl) }, Cmd.none )

        ShowPointsChecked chartId sp ->
            ( { model | charts = model.charts |> IdDict.update chartId (Tuple.mapSecond <| Graph.setShowPoints sp) }, Cmd.none )

        DataSetHovered chartId id ->
            ( { model | charts = model.charts |> IdDict.update chartId (Tuple.mapSecond <| Graph.hoverDataSet id) }, Cmd.none )

        DataSetClicked chartId id ->
            ( { model | charts = model.charts |> IdDict.update chartId (Tuple.mapSecond <| Graph.toggleDataSetSelected id) }, Cmd.none )

        DataSetVisibleClicked chartId id ->
            ( { model
                | charts =
                    model.charts
                        |> IdDict.update chartId
                            (Tuple.mapBoth
                                (\c ->
                                    case c of
                                        LineChart cd ->
                                            LineChart { cd | chartables = cd.chartables |> IdDict.update id (\d -> { d | visible = not d.visible }) }
                                )
                                (Graph.toggleDataSet id)
                            )
              }
            , Cmd.none
            )

        DataSetBringForwardClicked chartId id ->
            ( { model
                | charts =
                    model.charts
                        |> IdDict.update chartId
                            (Tuple.mapBoth
                                (\c ->
                                    case c of
                                        LineChart cd ->
                                            LineChart { cd | chartableOrder = cd.chartableOrder |> bringForward id }
                                )
                                (Graph.bringDataSetForward id)
                            )
              }
            , Cmd.none
            )

        DataSetPushBackClicked chartId id ->
            ( { model
                | charts =
                    model.charts
                        |> IdDict.update chartId
                            (Tuple.mapBoth
                                (\c ->
                                    case c of
                                        LineChart cd ->
                                            LineChart { cd | chartableOrder = cd.chartableOrder |> pushBack id }
                                )
                                (Graph.pushDataSetBack id)
                            )
              }
            , Cmd.none
            )

        GraphMsg chartId graphMsg ->
            ( { model | charts = model.charts |> IdDict.update chartId (Tuple.mapSecond <| Graph.update graphMsg) }, Cmd.none )

        UserDataChanged userData ->
            ( { model | charts = UserData.charts userData |> IdDict.map (\_ c -> ( c, initGraph model.today c (UserData.chartables userData) (UserData.trackables userData) )) }, Cmd.none )


pushBack : id -> List id -> List id
pushBack id ids =
    case ids of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            if x == id then
                x :: y :: rest

            else if y == id then
                y :: x :: rest

            else
                x :: pushBack id (y :: rest)


bringForward : id -> List id -> List id
bringForward id ids =
    case ids of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            if x == id then
                y :: x :: rest

            else
                x :: bringForward id (y :: rest)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "shadow-inner-t-md" ] <|
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
        ]
            ++ (model.charts
                    |> IdDict.map
                        (\id ( c, g ) ->
                            case c of
                                LineChart d ->
                                    viewLineChart id g d model.chartables
                        )
                    |> IdDict.values
               )


viewLineChart : ChartId -> Graph.Model ChartableId -> LineChartData -> ChartableDict -> Html Msg
viewLineChart chartId graph chart chartables =
    let
        viewChartables =
            chart.chartableOrder
                |> List.filterMap
                    (\id ->
                        IdDict.get id chartables
                            |> Maybe.map
                                (\cb ->
                                    ( id
                                    , cb
                                    , Maybe.withDefault False <| Maybe.map .visible <| IdDict.get id chart.chartables
                                    )
                                )
                    )
                |> List.map
                    (\( id, cb, visible ) ->
                        let
                            canPushBack =
                                List.head graph.dataOrder /= Just id

                            canBringForward =
                                (List.head << List.reverse) graph.dataOrder /= Just id
                        in
                        [ div
                            [ class "p-2 flex first:mt-0 items-center"
                            , classList
                                [ ( "bg-gray-300", graph.selectedDataSet == Just id || graph.hoveredDataSet == Just id )
                                ]
                            , onMouseEnter <|
                                case graph.selectedDataSet of
                                    Just _ ->
                                        NoOp

                                    _ ->
                                        DataSetHovered chartId (Just id)
                            , onMouseLeave <|
                                case graph.selectedDataSet of
                                    Just _ ->
                                        NoOp

                                    _ ->
                                        DataSetHovered chartId Nothing
                            , onClick (DataSetClicked chartId id)
                            ]
                            [ div
                                [ class "w-16 h-8 mr-4 flex-grow-0 flex-shrink-0"
                                ]
                                [ Graph.viewKey "w-full h-full" cb.colour
                                ]
                            , span [ class "mr-4" ] [ text cb.name ]
                            , button
                                [ class "ml-auto text-black"
                                , classList
                                    [ ( "text-opacity-30 cursor-default", not canPushBack )
                                    , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canPushBack )
                                    ]
                                , onClickStopPropagation (DataSetPushBackClicked chartId id)
                                , disabled (not canPushBack)
                                ]
                                [ icon "w-6 h-6" <| SolidArrowUp
                                ]
                            , button
                                [ class "ml-2 text-black"
                                , classList
                                    [ ( "text-opacity-30 cursor-default", not canBringForward )
                                    , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canBringForward )
                                    ]
                                , onClickStopPropagation (DataSetBringForwardClicked chartId id)
                                , disabled (not canBringForward)
                                ]
                                [ icon "w-6 h-6" <| SolidArrowDown
                                ]
                            , button
                                [ class "ml-2 text-black"
                                , class "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                , onClickStopPropagation (DataSetVisibleClicked chartId id)
                                ]
                                [ icon "w-6 h-6" <|
                                    if visible then
                                        SolidEye

                                    else
                                        SolidEyeSlash
                                ]
                            ]
                        ]
                    )
    in
    div []
        [ div [ class "mx-4 my-0 flex scrollable-parent", style "height" "300px" ]
            [ viewJustYAxis "flex-grow-0 flex-shrink-0" graph
            , viewScrollableContainer [ Html.map (GraphMsg chartId) <| viewLineGraph "h-full" graph ]
            ]
        , div [ class "m-4 mt-4 flex flex-wrap justify-end" ]
            [ label [ class "text-right whitespace-nowrap", for "fill-lines" ] [ text "Colour under curves" ]
            , input
                [ type_ "checkbox"
                , id "fill-lines"
                , class "ml-2"
                , onCheck (FillLinesChecked chartId)
                , checked graph.fillLines
                ]
                []
            , label [ class "ml-8 text-right whitespace-nowrap", for "show-points" ] [ text "Show data points" ]
            , input
                [ type_ "checkbox"
                , id "show-points"
                , class "ml-2"
                , onCheck (ShowPointsChecked chartId)
                , checked graph.showPoints
                ]
                []
            ]
        , div [ class "m-4 mt-4" ] <|
            List.concat viewChartables
        ]


viewScrollableContainer : List (Html msg) -> Html msg
viewScrollableContainer children =
    div [ class "relative flex-grow" ]
        [ node "scrollable-container"
            [ id "chart"
            , class "absolute overflow-x-scroll top-0 bottom-scrollbar"
            ]
            children
        ]


concatMaybes : List (Maybe a) -> List a
concatMaybes maybeXs =
    case maybeXs of
        (Just x) :: xs ->
            x :: concatMaybes xs

        _ :: xs ->
            concatMaybes xs

        _ ->
            []


onCheckStopPropagation : msg -> Attribute msg
onCheckStopPropagation msg =
    let
        alwaysStopPropagation m =
            ( m, True )
    in
    stopPropagationOn "change" (Decode.map alwaysStopPropagation (Decode.succeed msg))


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    let
        alwaysStopPropagation m =
            ( m, True )
    in
    stopPropagationOn "click" (Decode.map alwaysStopPropagation (Decode.succeed msg))


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    let
        alwaysPreventDefault m =
            ( m, True )
    in
    preventDefaultOn "click" (Decode.map alwaysPreventDefault (Decode.succeed msg))
