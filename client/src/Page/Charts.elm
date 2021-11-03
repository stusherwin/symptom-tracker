module Page.Charts exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Colour
import Controls
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Dictx
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onMouseEnter, onMouseLeave)
import Htmlx
import IdDict exposing (IdDict)
import Listx
import Maybe exposing (Maybe)
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable exposing (Chartable, ChartableDict, ChartableId)
import UserData.LineChart as LineChart exposing (LineChart, LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..), TrackableDict, TrackableId)


type alias Model =
    { today : Date
    , charts : IdDict LineChartId (Graph.Model ChartableId ChartableModel)
    , trackables : IdDict TrackableId String
    }


type alias ChartableModel =
    { trackables : Dict Int TrackableModel }


type alias TrackableModel =
    { id : TrackableId
    , multiplier : String
    , isValid : Bool
    }


init : Date -> UserData -> ( Model, Cmd Msg )
init today userData =
    ( { today = today
      , charts = UserData.lineCharts userData |> IdDict.map (\_ chart -> toModel today (UserData.chartables userData) (UserData.trackables userData) chart)
      , trackables = UserData.trackables userData |> IdDict.map (\_ t -> t.question)
      }
    , Cmd.batch <|
        IdDict.values <|
            (UserData.lineCharts userData
                |> IdDict.map
                    (\id _ ->
                        let
                            chartId =
                                "chart" ++ LineChart.idToString id
                        in
                        Dom.getViewportOf chartId
                            |> Task.andThen (\info -> Dom.setViewportOf chartId info.scene.width 0)
                            |> Task.attempt (always NoOp)
                    )
            )
    )


toModel : Date -> ChartableDict -> TrackableDict -> LineChart -> Graph.Model ChartableId ChartableModel
toModel today chartables trackables chart =
    let
        invert data =
            case List.maximum <| Dict.values data of
                Just max ->
                    data |> Dict.map (\_ v -> max - v)

                _ ->
                    data
    in
    { today = today
    , fillLines = chart.fillLines
    , showPoints = chart.showPoints
    , data =
        chartables
            |> IdDict.filter (\id _ -> List.member id <| IdDict.keys chart.chartables)
            |> IdDict.map
                (\id chartable ->
                    { name = chartable.name
                    , colour =
                        chartable.colour
                            |> Maybe.withDefault
                                (List.head chartable.sum
                                    |> Maybe.andThen ((\t -> IdDict.get t trackables) << Tuple.first)
                                    |> Maybe.map .colour
                                    |> Maybe.withDefault Colour.Blue
                                )
                    , dataPoints =
                        chartable.sum
                            |> List.filterMap
                                (\( trackableId, multiplier ) ->
                                    IdDict.get trackableId trackables
                                        |> Maybe.map
                                            (Dict.map (\_ v -> v * multiplier) << Trackable.onlyFloatData)
                                )
                            |> List.foldl (Dictx.unionWith (\v1 v2 -> v1 + v2)) Dict.empty
                            |> (if chartable.inverted then
                                    invert

                                else
                                    identity
                               )
                    , trackables =
                        chartable.sum
                            |> List.filterMap
                                (\( trackableId, multiplier ) ->
                                    IdDict.get trackableId trackables
                                        |> Maybe.map (\t -> { id = trackableId, multiplier = String.fromFloat multiplier, isValid = True })
                                )
                            |> List.indexedMap Tuple.pair
                            |> Dict.fromList
                    , visible = Maybe.withDefault False <| Maybe.map .visible <| IdDict.get id chart.chartables
                    }
                )
    , dataOrder = chart.chartableOrder
    , selectedDataSet = Nothing
    , hoveredDataSet = Nothing
    , selectedDataPoint = Nothing
    }



-- UPDATE


type Msg
    = NoOp
    | FillLinesChecked LineChartId Bool
    | ShowPointsChecked LineChartId Bool
    | DataSetHovered LineChartId (Maybe ChartableId)
    | DataSetClicked LineChartId ChartableId
    | DataSetVisibleClicked LineChartId ChartableId
    | DataSetBringForwardClicked LineChartId ChartableId
    | DataSetPushBackClicked LineChartId ChartableId
    | TrackableChanged LineChartId ChartableId Int (Maybe TrackableId)
    | TrackableMultiplierUpdated LineChartId ChartableId Int String
    | GraphMsg LineChartId (Graph.Msg ChartableId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FillLinesChecked chartId fl ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| \g -> { g | fillLines = fl }) }, Cmd.none )

        ShowPointsChecked chartId sp ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| \g -> { g | showPoints = sp }) }, Cmd.none )

        DataSetHovered chartId id ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| Graph.hoverDataSet id) }, Cmd.none )

        DataSetClicked chartId id ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| Graph.toggleDataSetSelected id) }, Cmd.none )

        DataSetVisibleClicked chartId id ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| Graph.toggleDataSet id) }, Cmd.none )

        DataSetBringForwardClicked chartId id ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| \g -> { g | dataOrder = g.dataOrder |> Listx.moveTailwards id }) }, Cmd.none )

        DataSetPushBackClicked chartId id ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| \g -> { g | dataOrder = g.dataOrder |> Listx.moveHeadwards id }) }, Cmd.none )

        TrackableChanged chartId id i (Just tId) ->
            ( { model
                | charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                \g ->
                                    { g
                                        | data =
                                            g.data
                                                |> (IdDict.update id <|
                                                        \ds ->
                                                            { ds
                                                                | trackables = ds.trackables |> Dict.update i (Maybe.map <| \t -> { t | id = tId })
                                                            }
                                                   )
                                    }
                           )
              }
            , Cmd.none
            )

        TrackableMultiplierUpdated chartId id i stringValue ->
            let
                isValid =
                    case String.toFloat stringValue of
                        Just multiplier ->
                            multiplier > 0

                        _ ->
                            False
            in
            ( { model
                | charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                \g ->
                                    { g
                                        | data =
                                            g.data
                                                |> (IdDict.update id <|
                                                        \ds ->
                                                            { ds
                                                                | trackables =
                                                                    ds.trackables
                                                                        |> (Dict.update i <|
                                                                                Maybe.map <|
                                                                                    \t ->
                                                                                        { t
                                                                                            | multiplier = stringValue
                                                                                            , isValid = isValid
                                                                                        }
                                                                           )
                                                            }
                                                   )
                                    }
                           )
              }
            , Cmd.none
            )

        GraphMsg chartId graphMsg ->
            ( { model | charts = model.charts |> (IdDict.update chartId <| Graph.update graphMsg) }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "shadow-inner-t-md" ] <|
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
        ]
            ++ (model.charts
                    |> IdDict.map (viewLineChart model.trackables)
                    |> IdDict.values
               )


viewLineChart : IdDict TrackableId String -> LineChartId -> Graph.Model ChartableId ChartableModel -> Html Msg
viewLineChart trackables chartId model =
    let
        viewChartables =
            model.dataOrder
                |> List.filterMap
                    (\id ->
                        IdDict.get id model.data |> Maybe.map (\ds -> ( id, ds ))
                    )
                |> List.map
                    (\( id, dataSet ) ->
                        let
                            canPushBack =
                                List.head model.dataOrder /= Just id

                            canBringForward =
                                (List.head << List.reverse) model.dataOrder /= Just id
                        in
                        [ div
                            [ class "p-4 border-t-4"
                            , Colour.class "bg" dataSet.colour
                            , Colour.classUp "border" dataSet.colour
                            , classList
                                [ ( "bg-opacity-50 border-opacity-50", not dataSet.visible )
                                ]
                            , onMouseEnter <| DataSetHovered chartId (Just id)
                            , onMouseLeave <| DataSetHovered chartId Nothing
                            ]
                            [ div
                                [ class "flex items-center"
                                , onClick (DataSetClicked chartId id)
                                ]
                                [ button
                                    [ class "text-black focus:outline-none"
                                    , classList
                                        [ ( "text-opacity-30 hover:text-opacity-50 focus:text-opacity-50", not dataSet.visible )
                                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", dataSet.visible )
                                        ]
                                    , Htmlx.onClickStopPropagation <| DataSetVisibleClicked chartId id
                                    ]
                                    [ icon "w-6 h-6" <|
                                        if dataSet.visible then
                                            SolidEye

                                        else
                                            SolidEyeSlash
                                    ]
                                , span
                                    [ class "ml-4 font-bold text-black"
                                    , classList
                                        [ ( "text-opacity-30", not dataSet.visible )
                                        , ( "text-opacity-100", dataSet.visible )
                                        ]
                                    ]
                                    [ text dataSet.name ]
                                , button
                                    [ class "ml-2 text-black focus:outline-none"
                                    , class "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                                    , Htmlx.onClickStopPropagation <| DataSetClicked chartId id
                                    ]
                                    [ icon "w-6 h-6" <|
                                        if model.selectedDataSet == Just id then
                                            SolidAngleUp

                                        else
                                            SolidAngleDown
                                    ]
                                , button
                                    [ class "ml-auto text-black focus:outline-none"
                                    , classList
                                        [ ( "text-opacity-0 cursor-default", not canPushBack )
                                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canPushBack )
                                        ]
                                    , Htmlx.onClickStopPropagation <| DataSetPushBackClicked chartId id
                                    , disabled (not canPushBack)
                                    ]
                                    [ icon "w-6 h-6" <| SolidArrowUp
                                    ]
                                , button
                                    [ class "ml-2 text-black focus:outline-none"
                                    , classList
                                        [ ( "text-opacity-0 cursor-default", not canBringForward )
                                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canBringForward )
                                        ]
                                    , Htmlx.onClickStopPropagation <| DataSetBringForwardClicked chartId id
                                    , disabled (not canBringForward)
                                    ]
                                    [ icon "w-6 h-6" <| SolidArrowDown
                                    ]
                                ]
                            , if model.selectedDataSet == Just id then
                                div [ class "mt-4" ] <|
                                    (Dict.values <|
                                        Dict.map
                                            (\i t ->
                                                div [ class "ml-1 mt-4 flex" ]
                                                    [ icon "mt-3 w-4 h-4 flex-grow-0 flex-shrink-0" <|
                                                        if i == 0 then
                                                            SolidEquals

                                                        else
                                                            SolidPlus
                                                    , Controls.textDropdown "ml-5 w-full" (TrackableChanged chartId id i) Trackable.idToString Trackable.idFromString (trackables |> IdDict.map (\tId q -> ( ( tId, True ), q )) |> IdDict.values) (Just t.id) { showFilled = False }
                                                    , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                                                    , Controls.textbox [ class "ml-4 w-24 flex-grow-0 flex-shrink-0" ] [] t.multiplier t.isValid (TrackableMultiplierUpdated chartId id i)
                                                    ]
                                            )
                                        <|
                                            dataSet.trackables
                                    )
                                        ++ [ div [ class "ml-10 mt-4 flex" ]
                                                [ Controls.button "" Controls.ButtonGrey NoOp SolidPlusCircle "Add trackable" True ]
                                           ]

                              else
                                div [] []
                            ]
                        ]
                    )
    in
    div []
        [ div [ class "mx-4 my-0 flex scrollable-parent", style "height" "300px" ]
            [ viewJustYAxis "flex-grow-0 flex-shrink-0" model
            , viewScrollableContainer ("chart" ++ LineChart.idToString chartId) [ Html.map (GraphMsg chartId) <| viewLineGraph "h-full" model ]
            ]
        , div [ class "m-4 mt-4 flex flex-wrap justify-end" ]
            [ label [ class "text-right whitespace-nowrap", for "fill-lines" ] [ text "Colour under curves" ]
            , input
                [ type_ "checkbox"
                , id "fill-lines"
                , class "ml-2"
                , onCheck (FillLinesChecked chartId)
                , checked model.fillLines
                ]
                []
            , label [ class "ml-8 text-right whitespace-nowrap", for "show-points" ] [ text "Show data points" ]
            , input
                [ type_ "checkbox"
                , id "show-points"
                , class "ml-2"
                , onCheck (ShowPointsChecked chartId)
                , checked model.showPoints
                ]
                []
            ]
        , div [ class "mt-4 bg-gray-200" ] <|
            List.concat viewChartables
        ]


viewScrollableContainer : String -> List (Html msg) -> Html msg
viewScrollableContainer containerId children =
    div [ class "relative flex-grow" ]
        [ node "scrollable-container"
            [ id containerId
            , class "absolute overflow-x-scroll top-0 bottom-scrollbar"
            ]
            children
        ]
