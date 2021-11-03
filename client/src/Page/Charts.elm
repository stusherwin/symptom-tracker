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
import Stringx
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as Chartable exposing (Chartable, ChartableDict, ChartableId)
import UserData.LineChart as LineChart exposing (LineChart, LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..), TrackableDict, TrackableId)


type alias Model =
    { today : Date
    , charts : List ( LineChartId, Graph.Model LineChartModel ChartableId ChartableModel )
    , userData : UserData
    , trackableOptions : List ( TrackableId, String )
    , chartableOptions : List ( ChartableId, String )
    }


type alias LineChartModel =
    { addingChartable : Bool
    , chartableToAdd : Maybe ChartableId
    , newChartable : Bool
    }


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
                |> IdDict.map (\_ chart -> toChartModel today (UserData.chartables userData) (UserData.trackables userData) chart)
                |> IdDict.toList
      , trackableOptions =
            UserData.trackables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond .question)
      , chartableOptions =
            UserData.chartables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .name))
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


toChartModel : Date -> ChartableDict -> TrackableDict -> LineChart -> Graph.Model LineChartModel ChartableId ChartableModel
toChartModel today chartables trackables chart =
    { today = today
    , fillLines = chart.fillLines
    , showPoints = chart.showPoints
    , data =
        chart.chartables
            |> IdDict.map (toChartableModel chartables trackables)
            |> IdDict.concatMaybes
    , dataOrder = chart.chartableOrder
    , selectedDataSet = Nothing
    , hoveredDataSet = Nothing
    , selectedDataPoint = Nothing
    , addingChartable = False
    , chartableToAdd = Nothing
    , newChartable = False
    }


toChartableModel : ChartableDict -> TrackableDict -> ChartableId -> { visible : Bool } -> Maybe (Graph.DataSet ChartableModel)
toChartableModel chartables trackables chartableId { visible } =
    chartables
        |> IdDict.get chartableId
        |> Maybe.map
            (\chartable ->
                { name = chartable.name
                , colour = toColour trackables chartable
                , dataPoints = toDataPoints trackables chartable
                , inverted = chartable.inverted
                , trackables = chartable.sum |> List.filterMap (toTrackableModel trackables)
                , visible = visible
                , nameIsPristine = True
                }
            )


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
                |> Maybe.withDefault Colour.Gray
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
    | ChartableHovered LineChartId (Maybe ChartableId)
    | ChartableEditClicked LineChartId ChartableId
    | ChartableCloseClicked LineChartId ChartableId
    | ChartableVisibleClicked LineChartId ChartableId
    | ChartableUpClicked LineChartId ChartableId
    | ChartableDownClicked LineChartId ChartableId
    | ChartableChanged LineChartId ChartableId (Maybe ChartableId)
    | ChartableNameUpdated LineChartId ChartableId String
    | ChartableInvertedChanged LineChartId ChartableId Bool
    | ChartableAddClicked LineChartId
    | ChartableToAddChanged LineChartId (Maybe ChartableId)
    | ChartableAddConfirmClicked LineChartId
    | ChartableAddCancelClicked LineChartId
    | ChartableDeleteClicked LineChartId ChartableId
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

        chartables =
            UserData.chartables model.userData

        updateChartModel chartId fn m =
            { m | charts = m.charts |> Listx.updateLookup chartId fn }

        updateChartableModel chartableId fn c =
            { c | data = c.data |> IdDict.update chartableId fn }

        updateUserData fn m =
            { m | userData = m.userData |> fn }
    in
    case msg of
        FillLinesChecked chartId fl ->
            ( model |> (updateChartModel chartId <| \c -> { c | fillLines = fl }), Cmd.none )

        ShowPointsChecked chartId sp ->
            ( model |> (updateChartModel chartId <| \c -> { c | showPoints = sp }), Cmd.none )

        ChartableHovered chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.hoverDataSet chartableId), Cmd.none )

        ChartableEditClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.toggleDataSetSelected chartableId)
            , Task.attempt (always NoOp) <| Dom.focus <| "chart" ++ LineChart.idToString chartId ++ "-chartable" ++ Chartable.idToString chartableId ++ "-name"
            )

        ChartableCloseClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| (Graph.toggleDataSetSelected chartableId << \c -> {c | newChartable = False })), Cmd.none )

        ChartableVisibleClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.toggleDataSet chartableId), Cmd.none )

        ChartableUpClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| \c -> { c | dataOrder = c.dataOrder |> Listx.moveHeadwards chartableId }), Cmd.none )

        ChartableDownClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| \c -> { c | dataOrder = c.dataOrder |> Listx.moveTailwards chartableId }), Cmd.none )

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
                            \c -> { c | name = name, nameIsPristine = False }
                   )
                |> (\m -> { m | chartableOptions = m.chartableOptions |> Listx.insertLookup chartableId (Stringx.withDefault "[no name]" name) })
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

        ChartableAddClicked chartId ->
            ( model
                |> (updateChartModel chartId <|
                        \c ->
                            { c
                                | addingChartable = True
                                , chartableToAdd =
                                    model.chartableOptions
                                        |> List.map Tuple.first
                                        |> List.filter (\tId -> not (List.member tId <| IdDict.keys c.data))
                                        |> List.head
                            }
                   )
            , Cmd.none
            )

        ChartableToAddChanged chartId chartableId ->
            ( model
                |> (updateChartModel chartId <|
                        \c ->
                            { c
                                | chartableToAdd = chartableId
                            }
                   )
            , Cmd.none
            )

        ChartableAddConfirmClicked chartId ->
            let
                ( chartableIdM, userDataU, isNew ) =
                    case model.charts |> Listx.lookup chartId |> Maybe.andThen .chartableToAdd of
                        Just id ->
                            ( Just id, model.userData, False )

                        _ ->
                            let (id, userData) = 
                                    model.userData
                                        |> UserData.addChartable
                                            { name = ""
                                            , colour = Nothing
                                            , inverted = False
                                            , sum = []
                                            }
                            in (id, userData, True)

                chartablesU =
                    UserData.chartables userDataU

                newChartableModelM =
                    chartableIdM |> Maybe.andThen (\chartableId -> toChartableModel chartablesU trackables chartableId { visible = True })
            in
            case ( chartableIdM, newChartableModelM ) of
                ( Just chartableId, Just newChartableModel ) ->
                    ( { model
                        | userData =
                            userDataU
                                |> (UserData.updateLineChart chartId <|
                                        \c ->
                                            { c
                                                | chartables = c.chartables |> IdDict.insert chartableId { visible = True }
                                                , chartableOrder = chartableId :: c.chartableOrder
                                            }
                                   )
                        , charts =
                            model.charts
                                |> (Listx.updateLookup chartId <|
                                        \c ->
                                            { c
                                                | data = c.data |> IdDict.insert chartableId newChartableModel
                                                , dataOrder = chartableId :: c.dataOrder
                                                , addingChartable = False
                                                , chartableToAdd = Nothing
                                                , selectedDataSet = if isNew then Just chartableId else Nothing
                                                , newChartable = isNew
                                            }
                                   )
                        , chartableOptions = model.chartableOptions |> Listx.insertLookup chartableId (Stringx.withDefault "[no name]" newChartableModel.name)
                      }
                    , if isNew then Task.attempt (always NoOp) <| Dom.focus <| "chart" ++ LineChart.idToString chartId ++ "-chartable" ++ Chartable.idToString chartableId ++ "-name"
                    else Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChartableAddCancelClicked chartId ->
            ( model
                |> (updateChartModel chartId <|
                        \c ->
                            { c
                                | addingChartable = False
                                , chartableToAdd = Nothing
                            }
                   )
            , Cmd.none
            )

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
                        (\c m -> { c | sum = c.sum |> Listx.insertLookup trackableId m })
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
                    |> List.map (viewLineChart model.chartableOptions model.trackableOptions)
               )


viewLineChart : List ( ChartableId, String ) -> List ( TrackableId, String ) -> ( LineChartId, Graph.Model LineChartModel ChartableId ChartableModel ) -> Html Msg
viewLineChart chartableOptions trackableOptions ( chartId, model ) =
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
                            canMoveUp =
                                List.head model.dataOrder /= Just chartableId

                            canMoveDown =
                                (List.head << List.reverse) model.dataOrder /= Just chartableId
                        in
                        [ div
                            [ class "border-t-4"
                            , Colour.class "bg" dataSet.colour
                            , Colour.classUp "border" dataSet.colour
                            , classList
                                [ ( "bg-opacity-50", not dataSet.visible )
                                ]
                            , onMouseEnter <| ChartableHovered chartId (Just chartableId)
                            , onMouseLeave <| ChartableHovered chartId Nothing
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
                                        , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartId chartableId
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
                                            [ ( "text-opacity-0 cursor-default", not canMoveUp )
                                            , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveUp )
                                            ]
                                        , Htmlx.onClickStopPropagation <| ChartableUpClicked chartId chartableId
                                        , disabled (not canMoveUp)
                                        ]
                                        [ icon "w-5 h-5" <| SolidArrowUp
                                        ]
                                    , button
                                        [ class "ml-1 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                                        , classList
                                            [ ( "text-opacity-0 cursor-default", not canMoveDown )
                                            , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveDown )
                                            ]
                                        , Htmlx.onClickStopPropagation <| ChartableDownClicked chartId chartableId
                                        , disabled (not canMoveDown)
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
                                        , onClick (ChartableEditClicked chartId chartableId)
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
                                        , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartId chartableId
                                        ]
                                        [ icon "w-5 h-5" <|
                                            if dataSet.visible then
                                                SolidEye

                                            else
                                                SolidEyeSlash
                                        ]
                                    , Controls.textbox [ class "ml-4 w-72" ] [ 
                                        id <| "chart" ++ LineChart.idToString chartId ++ "-chartable" ++ Chartable.idToString chartableId ++ "-name"
                                        , placeholder "Name" ] dataSet.name { isValid = True, isRequired = True, isPristine = dataSet.nameIsPristine } (ChartableNameUpdated chartId chartableId)
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
                                        , onClick (ChartableCloseClicked chartId chartableId)
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
                                                        |> List.sortBy (String.toUpper << Tuple.second)
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
                                                    Nothing
                                                    (Just trackableId)
                                                    { showFilled = False }
                                                , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                                                , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True, isPristine = False } (TrackableMultiplierUpdated chartId chartableId trackableId)
                                                , button
                                                    [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                                    , onClick (TrackableDeleteClicked chartId chartableId trackableId)
                                                    ]
                                                    [ icon "w-5 h-5" <| SolidTrashAlt ]
                                                ]
                                        )
                                )
                                    ++ [ div [ class "mt-4 first:mt-0 flex" ]
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
            [ if model.addingChartable then
                div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                    [ Controls.textDropdown "w-full h-10"
                        (ChartableToAddChanged chartId)
                        Chartable.idToString
                        Chartable.idFromString
                        (chartableOptions
                            |> List.sortBy (String.toUpper << Tuple.second)
                            |> List.map
                                (\( cId, name ) ->
                                    ( ( cId
                                      , not <|
                                            List.member cId <|
                                                model.dataOrder
                                      )
                                    , name
                                    )
                                )
                        )
                        (Just "New chartable")
                        model.chartableToAdd
                        { showFilled = False }
                    , Controls.button "ml-4" Controls.ButtonGrey (ChartableAddConfirmClicked chartId) SolidPlusCircle "Add" True
                    , button
                        [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                        , onClick (ChartableAddCancelClicked chartId)
                        ]
                        [ icon "w-5 h-5" <| SolidTimes ]
                    ]

              else if model.newChartable then
                div [] []

              else
                div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                    [ Controls.button "" Controls.ButtonGrey (ChartableAddClicked chartId) SolidPlusCircle "Add chartable" True
                    ]
            ]
                ++ List.concat viewChartables
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
