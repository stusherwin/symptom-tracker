module Page.Charts exposing (Model, Msg(..), init, update, view)

import Array
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
    , charts : List ( LineChartId, Graph.Model ChartableId ChartableModel )
    , userData : UserData
    , trackableOptions : List ( TrackableId, String )
    }


type alias ChartableModel =
    { trackables : List ( TrackableId, TrackableModel )
    , inverted : Bool
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
                |> IdDict.map (\_ chart -> toChartModel today (UserData.chartables userData) (UserData.trackables userData) chart)
                |> IdDict.toList
      , trackableOptions =
            UserData.trackables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond .question)
      , userData = userData
      }
    , Cmd.batch <|
        (UserData.lineCharts userData
            |> IdDict.keys
            |> List.map
                (\id ->
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
            |> (IdDict.map <|
                    \id chartable ->
                        { name = chartable.name
                        , colour = toColour trackables chartable
                        , dataPoints = toDataPoints trackables chartable
                        , inverted = chartable.inverted
                        , trackables = chartable.sum |> List.filterMap (toTrackableModel trackables)
                        , visible = Maybe.withDefault False <| Maybe.map .visible <| IdDict.get id chart.chartables
                        }
               )
    , dataOrder = chart.chartableOrder
    , selectedDataSet = Nothing
    , hoveredDataSet = Nothing
    , selectedDataPoint = Nothing
    }


toTrackableModel : TrackableDict -> ( TrackableId, Float ) -> Maybe ( TrackableId, TrackableModel )
toTrackableModel trackables ( trackableId, multiplier ) =
    trackables
        |> IdDict.get trackableId
        |> Maybe.map
            (\{ question } ->
                ( trackableId
                , { question = question
                  , multiplier = String.fromFloat multiplier
                  , isValid = True
                  }
                )
            )


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
    | TrackableChanged LineChartId ChartableId TrackableId (Maybe TrackableId)
    | TrackableMultiplierUpdated LineChartId ChartableId TrackableId String
    | TrackableAddClicked LineChartId ChartableId
    | TrackableDeleteClicked LineChartId ChartableId TrackableId
    | GraphMsg LineChartId (Graph.Msg ChartableId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        trackables =
            UserData.trackables model.userData

        updateChartModel chartId fn m =
            { m | charts = m.charts |> Listx.updateLookup chartId fn }

        updateChartableModel chartableId fn c =
            { c | data = c.data |> IdDict.update chartableId fn }

        updateTrackableModel i fn c =
            { c | trackables = c.trackables |> Dict.update i (Maybe.map fn) }

        updateUserData fn m =
            { m | userData = m.userData |> fn }
    in
    case msg of
        FillLinesChecked chartId fl ->
            ( model |> (updateChartModel chartId <| \c -> { c | fillLines = fl }), Cmd.none )

        ShowPointsChecked chartId sp ->
            ( model |> (updateChartModel chartId <| \c -> { c | showPoints = sp }), Cmd.none )

        DataSetHovered chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.hoverDataSet chartableId), Cmd.none )

        DataSetClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.toggleDataSetSelected chartableId), Cmd.none )

        DataSetVisibleClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.toggleDataSet chartableId), Cmd.none )

        DataSetBringForwardClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| \c -> { c | dataOrder = c.dataOrder |> Listx.moveTailwards chartableId }), Cmd.none )

        DataSetPushBackClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| \c -> { c | dataOrder = c.dataOrder |> Listx.moveHeadwards chartableId }), Cmd.none )

        ChartableNameUpdated chartId chartableId name ->
            ( model
                |> (updateUserData <|
                        if not <| String.isEmpty name then
                            UserData.updateChartable chartableId <| \c -> { c | name = name }

                        else
                            identity
                   )
                |> (updateChartModel chartId <|
                        updateChartableModel chartableId <|
                            \c -> { c | name = name }
                   )
            , Cmd.none
            )

        ChartableInvertedChanged chartId chartableId inverted ->
            let
                chartableUM =
                    UserData.chartables model.userData
                        |> IdDict.get chartableId
                        |> (Maybe.map <|
                                \c -> { c | inverted = inverted }
                           )
            in
            case chartableUM of
                Just chartableU ->
                    ( model
                        |> (updateUserData <| UserData.insertChartable chartableId chartableU)
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | inverted = inverted
                                            , dataPoints = toDataPoints trackables chartableU
                                        }
                           )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TrackableChanged chartId chartableId trackableId (Just newTrackableId) ->
            let
                questionM =
                    model.trackableOptions |> Listx.lookup newTrackableId

                chartableUM =
                    UserData.chartables model.userData
                        |> IdDict.get chartableId
                        |> (Maybe.map <|
                                \c ->
                                    { c | sum = c.sum |> Listx.updateLookupWithKey trackableId (\( _, multiplier ) -> ( newTrackableId, multiplier )) }
                           )
            in
            case ( questionM, chartableUM ) of
                ( Just question, Just chartableU ) ->
                    ( model
                        |> (updateUserData <| UserData.insertChartable chartableId chartableU)
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> Listx.updateLookupWithKey trackableId (\( _, t ) -> ( newTrackableId, { t | question = question } ))
                                            , colour = toColour trackables chartableU
                                            , dataPoints = toDataPoints trackables chartableU
                                        }
                           )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TrackableMultiplierUpdated chartId chartableId trackableId stringValue ->
            let
                multiplierM =
                    String.toFloat stringValue

                isValid =
                    multiplierM |> Maybe.map (\v -> v > 0) |> Maybe.withDefault False

                chartableUM =
                    Maybe.map2
                        (\c m -> { c | sum = c.sum |> Listx.replaceLookup trackableId m })
                        (UserData.chartables model.userData |> IdDict.get chartableId)
                        multiplierM
            in
            ( model
                |> (updateUserData <|
                        case chartableUM of
                            Just chartableU ->
                                UserData.insertChartable chartableId chartableU

                            _ ->
                                identity
                   )
                |> (updateChartModel chartId <|
                        updateChartableModel chartableId <|
                            \c ->
                                { c
                                    | trackables =
                                        c.trackables
                                            |> Listx.updateLookup trackableId (\t -> { t | multiplier = stringValue, isValid = isValid })
                                    , dataPoints =
                                        case chartableUM of
                                            Just chartableU ->
                                                toDataPoints trackables chartableU

                                            _ ->
                                                c.dataPoints
                                }
                   )
            , Cmd.none
            )

        TrackableAddClicked chartId chartableId ->
            let
                chartableM =
                    UserData.chartables model.userData |> IdDict.get chartableId

                trackableM =
                    chartableM
                        |> Maybe.map (List.map Tuple.first << .sum)
                        |> Maybe.map
                            (\tIds ->
                                model.trackableOptions
                                    |> List.map Tuple.first
                                    |> List.filter (\tId -> not (List.member tId tIds))
                            )
                        |> Maybe.andThen List.head
                        |> Maybe.map (\tId -> ( tId, 1.0 ))

                trackableModelM =
                    trackableM |> Maybe.andThen (toTrackableModel trackables)

                chartableUM =
                    Maybe.map2
                        (\c trackable -> { c | sum = c.sum |> Array.fromList |> Array.push trackable |> Array.toList })
                        chartableM
                        trackableM
            in
            case ( trackableModelM, chartableUM ) of
                ( Just trackableModel, Just chartable ) ->
                    ( model
                        |> (updateUserData <| UserData.insertChartable chartableId chartable)
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> Array.fromList |> Array.push trackableModel |> Array.toList
                                            , colour = toColour trackables chartable
                                            , dataPoints = toDataPoints trackables chartable
                                        }
                           )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TrackableDeleteClicked chartId chartableId trackableId ->
            let
                chartableUM =
                    UserData.chartables model.userData
                        |> IdDict.get chartableId
                        |> Maybe.map
                            (\c ->
                                { c | sum = c.sum |> (List.filter <| \( id, _ ) -> id /= trackableId) }
                            )
            in
            case chartableUM of
                Just chartableU ->
                    ( model
                        |> (updateUserData <| UserData.insertChartable chartableId chartableU)
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> (List.filter <| \( tId, _ ) -> tId /= trackableId)
                                            , dataPoints = toDataPoints trackables chartableU
                                        }
                           )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChartableDeleteClicked chartId chartableId ->
            ( model
                |> (updateUserData <|
                        UserData.updateLineChart chartId <|
                            \c ->
                                { c
                                    | chartables = c.chartables |> IdDict.delete chartableId
                                    , chartableOrder = c.chartableOrder |> List.filter (\cId -> cId /= chartableId)
                                }
                   )
                |> (updateChartModel chartId <|
                        \c ->
                            { c
                                | data = c.data |> IdDict.delete chartableId
                                , dataOrder = c.dataOrder |> List.filter (\cId -> cId /= chartableId)
                            }
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
    div [ class "shadow-inner-t-md" ] <|
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
        ]
            ++ (model.charts
                    |> List.map (viewLineChart model.trackableOptions)
               )


viewLineChart : List ( TrackableId, String ) -> ( LineChartId, Graph.Model ChartableId ChartableModel ) -> Html Msg
viewLineChart trackableOptions ( chartId, model ) =
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
                            [ class "border-t-4"
                            , Colour.class "bg" dataSet.colour
                            , Colour.classUp "border" dataSet.colour
                            , classList
                                [ ( "bg-opacity-50", not dataSet.visible )
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
                                    [ ( "bg-opacity-50", not dataSet.visible )
                                    ]
                                ]
                            <|
                                (dataSet.trackables
                                    |> List.indexedMap
                                        (\i ( trackableId, t ) ->
                                            div [ class "mt-4 first:mt-0 flex" ]
                                                [ icon "mt-3 w-4 h-4 ml-0.5 mr-0.5 flex-grow-0 flex-shrink-0" <|
                                                    if i == 0 then
                                                        SolidEquals

                                                    else
                                                        SolidPlus
                                                , Controls.textDropdown "ml-4 w-full h-10"
                                                    (TrackableChanged chartId chartableId trackableId)
                                                    Trackable.idToString
                                                    Trackable.idFromString
                                                    (trackableOptions
                                                        |> List.map
                                                            (\( tId, question ) ->
                                                                ( ( tId
                                                                  , tId
                                                                        == trackableId
                                                                        || (not <|
                                                                                List.member tId <|
                                                                                    (dataSet.trackables
                                                                                        |> List.map Tuple.first
                                                                                    )
                                                                           )
                                                                  )
                                                                , question
                                                                )
                                                            )
                                                    )
                                                    (Just trackableId)
                                                    { showFilled = False }
                                                , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                                                , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True } (TrackableMultiplierUpdated chartId chartableId trackableId)
                                                , button
                                                    [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                                    , onClick (TrackableDeleteClicked chartId chartableId trackableId)
                                                    ]
                                                    [ icon "w-5 h-5" <| SolidTrashAlt ]
                                                ]
                                        )
                                )
                                    ++ [ div [ class "mt-4 flex" ]
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
        , div [ class "mt-4 bg-gray-200" ] <|
            List.concat viewChartables
                ++ [ div [ class "bg-gray-300 border-t-4 border-gray-400 flex" ]
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
