module Page.Charts exposing (Model, Msg(..), init, update, view)

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
import UserData.Chartable exposing (Chartable, ChartableDict, ChartableId)
import UserData.LineChart as LineChart exposing (LineChart, LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..), TrackableDict, TrackableId)


type alias Model =
    { today : Date
    , charts : IdDict LineChartId (Graph.Model ChartableId ChartableModel)
    , userData : UserData
    }


type alias ChartableModel =
    { trackables : Dict Int TrackableModel
    , inverted : Bool
    }


type alias TrackableModel =
    { trackableId : TrackableId
    , multiplier : String
    , isValid : Bool
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
                    { name = chartable.name
                    , colour = toColour trackables chartable
                    , dataPoints = toDataPoints trackables chartable
                    , inverted = chartable.inverted
                    , trackables =
                        chartable.sum
                            |> List.map
                                (\( trackableId, multiplier ) ->
                                    { trackableId = trackableId, multiplier = String.fromFloat multiplier, isValid = True }
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


toColour : TrackableDict -> Chartable -> Colour
toColour trackables chartable =
    chartable.colour
        |> Maybe.withDefault
            (List.head chartable.sum
                |> Maybe.andThen ((\t -> IdDict.get t trackables) << Tuple.first)
                |> Maybe.map .colour
                |> Maybe.withDefault Colour.Blue
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
    | ChartableNameUpdated LineChartId ChartableId String
    | ChartableInvertedChanged LineChartId ChartableId Bool
    | TrackableChanged LineChartId ChartableId Int (Maybe TrackableId)
    | TrackableMultiplierUpdated LineChartId ChartableId Int String
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
            ( updateChartModel chartId <| Graph.toggleDataSetSelected chartableId, Cmd.none )

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

        TrackableChanged chartId chartableId i (Just trackableId) ->
            let
                userData =
                    model.userData
                        |> (UserData.updateChartable chartableId <|
                                \c ->
                                    { c | sum = c.sum |> List.indexedMap Tuple.pair |> Dict.fromList |> Dict.update i (Maybe.map (\( _, multiplier ) -> ( trackableId, multiplier ))) |> Dict.values }
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
                                            | trackables = c.trackables |> Dict.update i (Maybe.map <| \t -> { t | trackableId = trackableId })
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
                                            | trackables = c.trackables |> Dict.update i (Maybe.map <| \t -> { t | multiplier = stringValue, isValid = isValid })
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
                    |> IdDict.map (viewLineChart (UserData.trackables model.userData))
                    |> IdDict.values
               )


viewLineChart : TrackableDict -> LineChartId -> Graph.Model ChartableId ChartableModel -> Html Msg
viewLineChart trackables chartId model =
    let
        viewChartables =
            model.dataOrder
                |> List.filterMap
                    (\chartableId ->
                        IdDict.get chartableId model.data |> Maybe.map (\ds -> ( chartableId, ds ))
                    )
                |> List.map
                    (\( chartableId, dataSet ) ->
                        let
                            canPushBack =
                                List.head model.dataOrder /= Just chartableId

                            canBringForward =
                                (List.head << List.reverse) model.dataOrder /= Just chartableId
                        in
                        [ div
                            [ class "p-4 border-t-4"
                            , Colour.class "bg" dataSet.colour
                            , Colour.classUp "border" dataSet.colour
                            , classList
                                [ ( "bg-opacity-50 border-opacity-50", not dataSet.visible )
                                ]
                            , onMouseEnter <| DataSetHovered chartId (Just chartableId)
                            , onMouseLeave <| DataSetHovered chartId Nothing
                            ]
                            [ div
                                [ class "flex items-center"
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
                                , a
                                    [ href "#"
                                    , target "_self"
                                    , Htmlx.onClickPreventDefault (DataSetClicked chartId chartableId)
                                    , class "ml-4 w-full font-bold flex items-center text-black"
                                    , classList
                                        [ ( "text-opacity-30", not dataSet.visible )
                                        , ( "text-opacity-100", dataSet.visible )
                                        ]
                                    ]
                                    [ text <|
                                        if String.isEmpty dataSet.name then
                                            "[no name]"

                                        else
                                            dataSet.name
                                    , icon "ml-1 w-5 h-5" <|
                                        if model.selectedDataSet == Just chartableId then
                                            SolidAngleUp

                                        else
                                            SolidAngleDown
                                    ]
                                , button
                                    [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
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
                                ]
                            , if model.selectedDataSet == Just chartableId then
                                div [ class "mt-4" ] <|
                                    [ div [ class "mt-4 ml-8 flex items-center" ]
                                        [ Controls.textbox [ class "w-72" ] [] dataSet.name { isValid = True, isRequired = True } (ChartableNameUpdated chartId chartableId)
                                        , label [ class "ml-auto font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                                        , input
                                            [ type_ "checkbox"
                                            , id "inverted"
                                            , class "ml-2"
                                            , onCheck (ChartableInvertedChanged chartId chartableId)
                                            , checked dataSet.inverted
                                            ]
                                            []
                                        ]
                                    ]
                                        ++ (dataSet.trackables
                                                |> Dict.map
                                                    (\i t ->
                                                        div [ class "mt-4 flex" ]
                                                            [ icon "mt-3 w-4 h-4 flex-grow-0 flex-shrink-0" <|
                                                                if i == 0 then
                                                                    SolidEquals

                                                                else
                                                                    SolidPlus
                                                            , Controls.textDropdown "ml-4 w-full h-10"
                                                                (TrackableChanged chartId chartableId i)
                                                                Trackable.idToString
                                                                Trackable.idFromString
                                                                (trackables |> IdDict.map (\tId { question } -> ( ( tId, True ), question )) |> IdDict.values)
                                                                (Just t.trackableId)
                                                                { showFilled = False }
                                                            , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                                                            , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True } (TrackableMultiplierUpdated chartId chartableId i)
                                                            ]
                                                    )
                                                |> Dict.values
                                           )
                                        ++ [ div [ class "mt-4 flex" ]
                                                [ Controls.button "ml-8" Controls.ButtonGrey NoOp SolidPlusCircle "Add trackable" True ]
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
        , div [ class "mt-4 bg-white" ] <|
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
