module Page.Charts exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Arrayx
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
import IdDict exposing (IdDict)
import Listx
import Maybe exposing (Maybe)
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData, chartables)
import UserData.Chartable as Chartable exposing (Chartable, ChartableDict, ChartableId)
import UserData.LineChart as LineChart exposing (LineChart, LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..), TrackableDict, TrackableId)


type alias Model =
    { today : Date
    , charts : IdDict LineChartId (Graph.Model ChartableId ChartableModel)
    , userData : UserData
    }


type alias ChartableModel =
    { trackables : Array TrackableModel
    , inverted : Bool
    , trackableToAdd : Maybe TrackableOption
    , trackableOptions : List TrackableOption
    }


type alias TrackableModel =
    { trackableId : TrackableId
    , question : String
    , multiplier : String
    , isValid : Bool
    }


type alias TrackableOption =
    { trackableId : TrackableId
    , question : String
    }


init : Date -> UserData -> ( Model, Cmd Msg )
init today userData =
    ( { today = today
      , charts = UserData.lineCharts userData |> IdDict.map (\_ chart -> toChartModel today (UserData.chartables userData) (UserData.trackables userData) chart)
      , userData = userData
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


toChartModel : Date -> ChartableDict -> TrackableDict -> LineChart -> Graph.Model ChartableId ChartableModel
toChartModel today chartables trackables chart =
    { today = today
    , fillLines = chart.fillLines
    , showPoints = chart.showPoints
    , data =
        chartables
            |> IdDict.filter (\id _ -> List.member id <| IdDict.keys chart.chartables)
            |> IdDict.map
                (\id chartable ->
                    let
                        allTrackables =
                            trackables
                                |> IdDict.map
                                    (\trackableId { question } ->
                                        { trackableId = trackableId
                                        , question = question
                                        }
                                    )
                                |> IdDict.values

                        thisTrackables =
                            chartable.sum
                                |> List.filterMap
                                    (\( trackableId, multiplier ) ->
                                        trackables
                                            |> IdDict.get trackableId
                                            |> Maybe.map
                                                (\{ question } ->
                                                    { trackableId = trackableId
                                                    , question = question
                                                    , multiplier = String.fromFloat multiplier
                                                    , isValid = True
                                                    }
                                                )
                                    )
                                |> Array.fromList
                    in
                    { name = chartable.name
                    , colour = toColour trackables chartable
                    , dataPoints = toDataPoints trackables chartable
                    , inverted = chartable.inverted
                    , trackables = thisTrackables
                    , visible = Maybe.withDefault False <| Maybe.map .visible <| IdDict.get id chart.chartables
                    , trackableToAdd = Nothing
                    , trackableOptions =
                        allTrackables
                            |> List.filter
                                (\{ trackableId } ->
                                    not <|
                                        List.member trackableId <|
                                            (thisTrackables
                                                |> Array.map .trackableId
                                                |> Array.toList
                                            )
                                )
                    }
                )
    , dataOrder = chart.chartableOrder
    , selectedDataSet = Nothing
    , hoveredDataSet = Nothing
    , selectedDataPoint = Nothing
    }


toColour : TrackableDict -> Chartable -> Colour
toColour trackables chartable =
    chartable.colour
        |> Maybe.withDefault
            (List.head chartable.sum
                |> Maybe.andThen ((\t -> IdDict.get t trackables) << Tuple.first)
                |> Maybe.map .colour
                |> Maybe.withDefault Colour.Black
            )


toDataPoints : TrackableDict -> Chartable -> Dict Int Float
toDataPoints trackables chartable =
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
    | ChartableChanged LineChartId Int (Maybe ChartableId)
    | ChartableNameUpdated LineChartId ChartableId String
    | ChartableAddClicked LineChartId
    | ChartableDeleteClicked LineChartId ChartableId
    | ChartableInvertedChanged LineChartId ChartableId Bool
    | TrackableMultiplierUpdated LineChartId ChartableId Int String
    | TrackableAddClicked LineChartId ChartableId
    | TrackableToAddChanged LineChartId ChartableId (Maybe TrackableId)
    | TrackableToAddCancelClicked LineChartId ChartableId
    | TrackableDeleteClicked LineChartId ChartableId Int
    | GraphMsg LineChartId (Graph.Msg ChartableId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChartModel chartId fn =
            { model | charts = model.charts |> IdDict.update chartId fn }

        updateChartableModel chartableId fn c =
            { c | data = c.data |> IdDict.update chartableId fn }

        updateTrackableModel i fn c =
            { c | trackables = c.trackables |> Dict.update i (Maybe.map fn) }
    in
    case msg of
        FillLinesChecked chartId fl ->
            ( updateChartModel chartId <| \c -> { c | fillLines = fl }, Cmd.none )

        ShowPointsChecked chartId sp ->
            ( updateChartModel chartId <| \c -> { c | showPoints = sp }, Cmd.none )

        DataSetHovered chartId chartableId ->
            ( updateChartModel chartId <| Graph.hoverDataSet chartableId, Cmd.none )

        DataSetClicked chartId chartableId ->
            ( { model
                | charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                (Graph.toggleDataSetSelected chartableId
                                    << (updateChartableModel chartableId <|
                                            \c ->
                                                { c
                                                    | trackableToAdd = Nothing
                                                }
                                       )
                                )
                           )
              }
            , Cmd.none
            )

        DataSetVisibleClicked chartId chartableId ->
            ( updateChartModel chartId <| Graph.toggleDataSet chartableId, Cmd.none )

        DataSetBringForwardClicked chartId chartableId ->
            ( updateChartModel chartId <| \c -> { c | dataOrder = c.dataOrder |> Listx.moveTailwards chartableId }, Cmd.none )

        DataSetPushBackClicked chartId chartableId ->
            ( updateChartModel chartId <| \c -> { c | dataOrder = c.dataOrder |> Listx.moveHeadwards chartableId }, Cmd.none )

        ChartableNameUpdated chartId chartableId name ->
            let
                userData =
                    if not <| String.isEmpty name then
                        model.userData
                            |> (UserData.updateChartable chartableId <|
                                    \c ->
                                        { c | name = name }
                               )

                    else
                        model.userData
            in
            ( { model
                | userData = userData
                , charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                updateChartableModel chartableId <|
                                    \c -> { c | name = name }
                           )
              }
            , Cmd.none
            )

        ChartableInvertedChanged chartId chartableId inverted ->
            let
                userData =
                    model.userData
                        |> (UserData.updateChartable chartableId <|
                                \c ->
                                    { c | inverted = inverted }
                           )
            in
            ( { model
                | userData = userData
                , charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | inverted = inverted
                                            , dataPoints =
                                                case UserData.chartables userData |> IdDict.get chartableId of
                                                    Just chartable ->
                                                        toDataPoints (UserData.trackables userData) chartable

                                                    _ ->
                                                        c.dataPoints
                                        }
                           )
              }
            , Cmd.none
            )

        TrackableMultiplierUpdated chartId chartableId i stringValue ->
            let
                ( isValid, userData ) =
                    case String.toFloat stringValue of
                        Just multiplier ->
                            if multiplier > 0 then
                                ( True
                                , model.userData
                                    |> (UserData.updateChartable chartableId <|
                                            \c ->
                                                { c | sum = c.sum |> List.indexedMap Tuple.pair |> Dict.fromList |> Dict.update i (Maybe.map (\( trackableId, _ ) -> ( trackableId, multiplier ))) |> Dict.values }
                                       )
                                )

                            else
                                ( False, model.userData )

                        _ ->
                            ( False, model.userData )
            in
            ( { model
                | userData = userData
                , charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> Arrayx.update i (\t -> { t | multiplier = stringValue, isValid = isValid })
                                            , dataPoints =
                                                case ( isValid, UserData.chartables userData |> IdDict.get chartableId ) of
                                                    ( True, Just chartable ) ->
                                                        toDataPoints (UserData.trackables userData) chartable

                                                    _ ->
                                                        c.dataPoints
                                        }
                           )
              }
            , Cmd.none
            )

        TrackableAddClicked chartId chartableId ->
            let
                trackableToAdd =
                    model.charts
                        |> IdDict.get chartId
                        |> Maybe.andThen (IdDict.get chartableId << .data)
                        |> Maybe.andThen .trackableToAdd
            in
            case trackableToAdd of
                Just t ->
                    let
                        userData =
                            model.userData
                                |> (UserData.updateChartable chartableId <|
                                        \c -> { c | sum = c.sum |> Array.fromList |> Array.push ( t.trackableId, 1.0 ) |> Array.toList }
                                   )
                    in
                    ( { model
                        | userData = userData
                        , charts =
                            model.charts
                                |> (IdDict.update chartId <|
                                        updateChartableModel chartableId <|
                                            \c ->
                                                { c
                                                    | trackables =
                                                        c.trackables
                                                            |> Array.push
                                                                { trackableId = t.trackableId
                                                                , question = t.question
                                                                , multiplier = "1"
                                                                , isValid = True
                                                                }
                                                    , colour =
                                                        case UserData.chartables userData |> IdDict.get chartableId of
                                                            Just chartable ->
                                                                toColour (UserData.trackables userData) chartable

                                                            _ ->
                                                                c.colour
                                                    , dataPoints =
                                                        case UserData.chartables userData |> IdDict.get chartableId of
                                                            Just chartable ->
                                                                toDataPoints (UserData.trackables userData) chartable

                                                            _ ->
                                                                c.dataPoints
                                                    , trackableOptions = c.trackableOptions |> List.filter (\{ trackableId } -> trackableId /= t.trackableId)
                                                    , trackableToAdd = Nothing
                                                }
                                   )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | charts =
                            model.charts
                                |> (IdDict.update chartId <|
                                        updateChartableModel chartableId <|
                                            \c ->
                                                { c
                                                    | trackableToAdd = List.head c.trackableOptions
                                                }
                                   )
                      }
                    , Cmd.none
                    )

        TrackableToAddChanged chartId chartableId (Just trackableId) ->
            ( { model
                | charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackableToAdd = c.trackableOptions |> List.filter (\t -> t.trackableId == trackableId) |> List.head
                                        }
                           )
              }
            , Cmd.none
            )

        TrackableToAddCancelClicked chartId chartableId ->
            ( { model
                | charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackableToAdd = Nothing
                                        }
                           )
              }
            , Cmd.none
            )

        TrackableDeleteClicked chartId chartableId i ->
            let
                userData =
                    model.userData
                        |> (UserData.updateChartable chartableId <|
                                \c ->
                                    { c | sum = c.sum |> Array.fromList |> Arrayx.delete i |> Array.toList }
                           )
            in
            ( { model
                | userData = userData
                , charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> Arrayx.delete i
                                            , dataPoints =
                                                case UserData.chartables userData |> IdDict.get chartableId of
                                                    Just chartable ->
                                                        toDataPoints (UserData.trackables userData) chartable

                                                    _ ->
                                                        c.dataPoints
                                        }
                           )
              }
            , Cmd.none
            )

        ChartableDeleteClicked chartId chartableId ->
            let
                userData =
                    model.userData
                        |> (UserData.updateLineChart chartId <|
                                \c ->
                                    { c
                                        | chartables = c.chartables |> IdDict.delete chartableId
                                        , chartableOrder = c.chartableOrder |> List.filter (\cId -> cId /= chartableId)
                                    }
                           )
            in
            ( { model
                | userData = userData
                , charts =
                    model.charts
                        |> (IdDict.update chartId <|
                                \c ->
                                    { c
                                        | data = c.data |> IdDict.delete chartableId
                                        , dataOrder = c.dataOrder |> List.filter (\cId -> cId /= chartableId)
                                    }
                           )
              }
            , Cmd.none
            )

        GraphMsg chartId graphMsg ->
            ( updateChartModel chartId <| Graph.update graphMsg, Cmd.none )

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
                    |> IdDict.map (viewLineChart (UserData.chartables model.userData) (UserData.trackables model.userData))
                    |> IdDict.values
               )


viewLineChart : ChartableDict -> TrackableDict -> LineChartId -> Graph.Model ChartableId ChartableModel -> Html Msg
viewLineChart chartables trackables chartId model =
    let
        viewChartables =
            model.dataOrder
                |> List.filterMap
                    (\chartableId ->
                        IdDict.get chartableId model.data |> Maybe.map (\ds -> ( chartableId, ds ))
                    )
                |> List.indexedMap
                    (\chartableIndex ( chartableId, dataSet ) ->
                        let
                            canPushBack =
                                List.head model.dataOrder /= Just chartableId

                            canBringForward =
                                (List.head << List.reverse) model.dataOrder /= Just chartableId
                        in
                        [ div
                            [ class "border-t-4"
                            , Colour.class "bg" dataSet.colour
                            , Colour.classUp "border" dataSet.colour
                            , classList
                                [ ( "bg-opacity-50 border-opacity-50", not dataSet.visible )
                                ]
                            , onMouseEnter <| DataSetHovered chartId (Just chartableId)
                            , onMouseLeave <| DataSetHovered chartId Nothing
                            ]
                            [ if model.selectedDataSet /= Just chartableId then
                                div
                                    [ class "p-4 flex items-center"
                                    ]
                                    [ button
                                        [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0"
                                        , classList
                                            [ ( "text-opacity-30 hover:text-opacity-50 focus:text-opacity-50", not dataSet.visible )
                                            , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", dataSet.visible )
                                            ]
                                        , Htmlx.onClickStopPropagation <| DataSetVisibleClicked chartId chartableId
                                        ]
                                        [ icon "w-5 h-5" <|
                                            if dataSet.visible then
                                                SolidEye

                                            else
                                                SolidEyeSlash
                                        ]
                                    , span [ class "ml-4 font-bold" ]
                                        [ text <|
                                            if String.isEmpty dataSet.name then
                                                "[no name]"

                                            else
                                                dataSet.name
                                        ]
                                    , button
                                        [ class "ml-auto flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                                        , classList
                                            [ ( "text-opacity-0 cursor-default", not canPushBack )
                                            , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canPushBack )
                                            ]
                                        , Htmlx.onClickStopPropagation <| DataSetPushBackClicked chartId chartableId
                                        , disabled (not canPushBack)
                                        ]
                                        [ icon "w-5 h-5" <| SolidArrowUp
                                        ]
                                    , button
                                        [ class "ml-1 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                                        , classList
                                            [ ( "text-opacity-0 cursor-default", not canBringForward )
                                            , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canBringForward )
                                            ]
                                        , Htmlx.onClickStopPropagation <| DataSetBringForwardClicked chartId chartableId
                                        , disabled (not canBringForward)
                                        ]
                                        [ icon "w-5 h-5" <| SolidArrowDown
                                        ]
                                    , button
                                        [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , onClick (ChartableDeleteClicked chartId chartableId)
                                        ]
                                        [ icon "w-5 h-5" <| SolidTrashAlt ]
                                    , button
                                        [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , onClick (DataSetClicked chartId chartableId)
                                        ]
                                        [ icon "w-5 h-5" <| SolidPencilAlt ]
                                    ]

                              else
                                div
                                    [ class "px-4 py-2 flex items-center"
                                    ]
                                    [ button
                                        [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0"
                                        , classList
                                            [ ( "text-opacity-30 hover:text-opacity-50 focus:text-opacity-50", not dataSet.visible )
                                            , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", dataSet.visible )
                                            ]
                                        , Htmlx.onClickStopPropagation <| DataSetVisibleClicked chartId chartableId
                                        ]
                                        [ icon "w-5 h-5" <|
                                            if dataSet.visible then
                                                SolidEye

                                            else
                                                SolidEyeSlash
                                        ]
                                    , Controls.textbox [ class "ml-4 w-72" ] [] dataSet.name { isValid = True, isRequired = True } (ChartableNameUpdated chartId chartableId)
                                    , label [ class "ml-auto font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                                    , input
                                        [ type_ "checkbox"
                                        , id "inverted"
                                        , class "ml-2"
                                        , onCheck (ChartableInvertedChanged chartId chartableId)
                                        , checked dataSet.inverted
                                        ]
                                        []
                                    , button
                                        [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , onClick (DataSetClicked chartId chartableId)
                                        ]
                                        [ icon "w-5 h-5" <| SolidTimes ]
                                    ]
                            ]
                        , if model.selectedDataSet == Just chartableId then
                            div
                                [ class "p-4"
                                , Colour.classDown "bg" dataSet.colour
                                , classList
                                    [ ( "bg-opacity-50 border-opacity-50", not dataSet.visible )
                                    ]
                                ]
                            <|
                                (dataSet.trackables
                                    |> Array.indexedMap
                                        (\trackableIndex t ->
                                            div [ class "mt-4 first:mt-0 flex" ]
                                                [ icon "mt-3 w-4 h-4 ml-0.5 mr-0.5 flex-grow-0 flex-shrink-0" <|
                                                    if trackableIndex == 0 then
                                                        SolidEquals

                                                    else
                                                        SolidPlus
                                                , span [ class "mt-2 ml-4 font-bold" ]
                                                    [ text t.question
                                                    ]
                                                , icon "mt-3 ml-auto w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                                                , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True } (TrackableMultiplierUpdated chartId chartableId trackableIndex)
                                                , button
                                                    [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                                    , onClick (TrackableDeleteClicked chartId chartableId trackableIndex)
                                                    ]
                                                    [ icon "w-5 h-5" <| SolidTrashAlt ]
                                                ]
                                        )
                                    |> Array.toList
                                )
                                    ++ [ div [ class "mt-4 flex" ] <|
                                            case dataSet.trackableToAdd of
                                                Just { trackableId } ->
                                                    [ icon "ml-4 mt-3 w-4 h-4 ml-0.5 mr-0.5 flex-grow-0 flex-shrink-0" SolidPlus
                                                    , Controls.textDropdown "ml-4 w-full h-10"
                                                        (TrackableToAddChanged chartId chartableId)
                                                        Trackable.idToString
                                                        Trackable.idFromString
                                                        (dataSet.trackableOptions
                                                            |> List.map (\t -> ( ( t.trackableId, True ), t.question ))
                                                        )
                                                        (Just trackableId)
                                                        { showFilled = False }
                                                    , Controls.button "ml-4 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey (TrackableAddClicked chartId chartableId) SolidPlusCircle "Add" True
                                                    , Controls.button "ml-2 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey (TrackableToAddCancelClicked chartId chartableId) SolidTimesCircle "Cancel" True
                                                    ]

                                                _ ->
                                                    [ Controls.button "ml-9 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey (TrackableAddClicked chartId chartableId) SolidPlusCircle "Add trackable" True ]
                                       ]

                          else
                            div [] []
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
        , div [ class "mt-4 bg-white" ] <|
            List.concat viewChartables
                ++ [ div [ class "bg-gray-500 border-t-4 border-gray-600 flex" ]
                        [ Controls.button "m-4" Controls.ButtonGrey (ChartableAddClicked chartId) SolidPlusCircle "Add chartable" True
                        ]
                   ]
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
