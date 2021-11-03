module Page.Chart exposing (Model, Msg(..), init, subscriptions, update, view)

import Array
import Browser.Dom as Dom
import Chart.Chartable
import Chart.LineChart as Chart exposing (DataSetId(..))
import Chart.Trackable
import Colour exposing (Colour)
import Controls
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onMouseEnter, onMouseLeave)
import Htmlx
import IdDict
import Listx
import Maybe exposing (Maybe)
import Process
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as Chartable exposing (Chartable)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChart as LineChart exposing (LineChart, LineChartData(..))
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable exposing (TrackableData(..))
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Model =
    { chartId : LineChartId
    , chart : Chart.Model
    , chartableOptions : List ( ( DataOption, Bool ), String )
    , addState : EditState
    , userData : UserData
    , data : List ( DataSetId, DataSet )
    , nameIsPristine : Bool

    -- , trackableOptions : List ( TrackableId, ( String, Bool ) )
    }


type DataOption
    = ChartableOption ChartableId
    | TrackableOption TrackableId



-- type DataSetId = ChartableId ChartableId
--                | TrackableId TrackableId


type DataSet
    = Chartable Chart.Chartable.Model
    | Trackable Chart.Trackable.Model


type EditState
    = NotAdding
    | AddingChartable (Maybe DataOption)


init : Date -> UserData -> LineChartId -> LineChart -> ( Model, Cmd Msg )
init today userData chartId chart =
    let
        ( chartModel, chartCmd ) =
            Chart.init today userData chartId chart

        trackableOptions =
            buildTrackableOptions userData chartId
    in
    ( { chartId = chartId
      , chart = chartModel
      , chartableOptions = buildChartableOptions userData chartId
      , addState = NotAdding
      , userData = userData
      , data =
            chart.data
                |> Array.toList
                |> List.filterMap
                    (\( data, visible ) ->
                        case data of
                            LineChart.Chartable chartableId ->
                                userData
                                    |> UserData.getChartable chartableId
                                    |> Maybe.map (\c -> ( ChartableId chartableId, Chartable <| Chart.Chartable.init userData True chartableId ( c, visible ) ))

                            LineChart.Trackable { id, multiplier, inverted } ->
                                userData
                                    |> UserData.getTrackable id
                                    |> Maybe.map (\t -> ( TrackableId id, Trackable <| Chart.Trackable.init trackableOptions True t multiplier inverted visible ))
                    )
      , nameIsPristine = True

      --   , trackableOptions = trackableOptions
      }
    , Cmd.batch
        [ Task.attempt (always NoOp) <| Dom.focus <| "chart-name"
        , Cmd.map ChartMsg chartCmd
        ]
    )


buildChartableOptions : UserData -> LineChartId -> List ( ( DataOption, Bool ), String )
buildChartableOptions userData chartId =
    let
        trackablesInUse =
            UserData.getLineChart chartId userData
                |> Maybe.map (Array.toList << .data)
                |> Maybe.withDefault []
                |> List.map Tuple.first
                |> List.filterMap
                    (\dataSetId ->
                        case dataSetId of
                            LineChart.Trackable { id } ->
                                Just id

                            _ ->
                                Nothing
                    )

        chartablesInUse =
            UserData.getLineChart chartId userData
                |> Maybe.map (Array.toList << .data)
                |> Maybe.withDefault []
                |> List.map Tuple.first
                |> List.filterMap
                    (\dataSetId ->
                        case dataSetId of
                            LineChart.Chartable id ->
                                Just id

                            _ ->
                                Nothing
                    )
    in
    (UserData.activeTrackables userData
        |> List.filter
            (\( _, ( t, _ ) ) ->
                case t.data of
                    TText _ ->
                        False

                    _ ->
                        True
            )
        |> (List.filter <| Tuple.second << Tuple.second)
        |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .question << Tuple.first))
        |> (List.map <| \( tId, name ) -> ( ( TrackableOption tId, not <| List.member tId trackablesInUse ), name ))
        |> (List.sortBy <| String.toUpper << Tuple.second)
    )
        ++ (UserData.activeChartables userData
                |> (List.filter <| Tuple.second << Tuple.second)
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .name << Tuple.first))
                |> (List.map <| \( cId, name ) -> ( ( ChartableOption cId, not <| List.member cId chartablesInUse ), name ))
                |> (List.sortBy <| String.toUpper << Tuple.second)
           )


buildTrackableOptions : UserData -> LineChartId -> List ( TrackableId, ( String, Bool ) )
buildTrackableOptions userData chartId =
    let
        trackablesInUse =
            UserData.getLineChart chartId userData
                |> Maybe.map (Array.toList << .data)
                |> Maybe.withDefault []
                |> List.map Tuple.first
                |> List.filterMap
                    (\dataSetId ->
                        case dataSetId of
                            LineChart.Trackable { id } ->
                                Just id

                            _ ->
                                Nothing
                    )
    in
    UserData.activeTrackables userData
        |> List.filter
            (\( _, ( t, _ ) ) ->
                case t.data of
                    TText _ ->
                        False

                    _ ->
                        True
            )
        |> (List.filter <| Tuple.second << Tuple.second)
        |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .question << Tuple.first))
        |> (List.map <| \( tId, name ) -> ( tId, ( name, not <| List.member tId trackablesInUse ) ))
        |> (List.sortBy <| String.toUpper << Tuple.first << Tuple.second)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChartMsg (Chart.subscriptions model.chart)



-- UPDATE


type Msg
    = NoOp
    | ChartMsg Chart.Msg
    | ChartableMsg Int Chart.Chartable.Msg
    | TrackableMsg Int Chart.Trackable.Msg
    | ChartNameUpdated String
    | ChartableAddClicked
    | ChartableToAddChanged (Maybe DataOption)
    | ChartableAddConfirmClicked
    | ChartableAddCancelClicked
    | UserDataUpdated UserData
    | MoveDataClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChart fn m =
            { m | chart = m.chart |> fn }

        updateChart1 : (Chart.Model -> ( Chart.Model, c )) -> Model -> ( Model, c )
        updateChart1 fn m =
            let
                ( m_, c ) =
                    m.chart |> fn
            in
            ( { m | chart = m_ }, c )

        updateChartable chartableId fn m =
            { m
                | data =
                    m.data
                        |> Listx.updateLookup (ChartableId chartableId)
                            (\d ->
                                case d of
                                    Chartable c ->
                                        Chartable (fn c)

                                    _ ->
                                        d
                            )
            }

        updateTrackable trackableId fn m =
            { m
                | data =
                    m.data
                        |> Listx.updateLookup (TrackableId trackableId)
                            (\d ->
                                case d of
                                    Trackable t ->
                                        Trackable (fn t)

                                    _ ->
                                        d
                            )
            }

        updateTrackableOptions userData chartId m =
            { m
                | data =
                    m.data
                        |> List.map
                            (Tuple.mapSecond
                                (\d ->
                                    case d of
                                        Trackable t ->
                                            Trackable { t | options = buildTrackableOptions userData chartId }

                                        _ ->
                                            d
                                )
                            )
            }

        updateChartableOptions userData chartId m =
            { m | chartableOptions = buildChartableOptions userData chartId }

        setUserData userData_ m =
            { m | userData = userData_ }
    in
    case msg of
        ChartNameUpdated name ->
            let
                chart =
                    model.chart

                userData_ =
                    model.userData
                        |> (if not <| String.isEmpty name then
                                UserData.updateLineChart model.chartId (LineChart.setName name)

                            else
                                identity
                           )
            in
            ( model
                |> setUserData userData_
                |> (\m -> { m | chart = { chart | name = name }, nameIsPristine = False })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableMsg i chartableMsg ->
            case chartableMsg of
                Chart.Chartable.ChartableHovered chartableId ->
                    ( model |> (updateChart <| Chart.hoverDataSet (chartableId |> Maybe.map ChartableId)), Cmd.none )

                Chart.Chartable.ChartableEditClicked chartableId ->
                    ( model |> (updateChart <| Chart.selectDataSet (Just (ChartableId chartableId)))
                    , Dom.focus ("chartable" ++ ChartableId.toString chartableId ++ "-name")
                        |> Task.attempt (always NoOp)
                    )

                Chart.Chartable.ChartableCloseClicked ->
                    ( model
                        |> (updateChart <| \c -> { c | expandedValue = False })
                        |> (updateChart <| Chart.selectDataSet Nothing)
                    , Cmd.none
                    )

                Chart.Chartable.ChartableVisibleClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.toggleDataVisible i)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.toggleDataSetVisible (ChartableId chartableId))
                        |> (updateChartable chartableId <| \c -> { c | visible = not c.visible })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableUpClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataUp i)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.moveDataSetBack (ChartableId chartableId))
                        |> (\m -> { m | data = m.data |> Listx.moveHeadwardsBy Tuple.first (ChartableId chartableId) })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableDownClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataDown i)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.moveDataSetForward (ChartableId chartableId))
                        |> (\m -> { m | data = m.data |> Listx.moveTailwardsBy Tuple.first (ChartableId chartableId) })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableNameUpdated chartableId name ->
                    let
                        userData_ =
                            model.userData
                                |> (if not <| String.isEmpty name then
                                        UserData.updateChartable chartableId (Chartable.setName name)

                                    else
                                        identity
                                   )
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChartable chartableId <| \c -> { c | name = name, nameIsPristine = False })
                        |> updateChartableOptions userData_ model.chartId
                        |> (updateChart <| Chart.updateDataSetName (ChartableId chartableId) name)
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableColourUpdated chartableId colour ->
                    let
                        userData_ =
                            model.userData
                                |> UserData.updateChartable chartableId (Chartable.setColour colour)

                        chartableM_ =
                            userData_ |> UserData.getChartable chartableId
                    in
                    case chartableM_ of
                        Just chartable_ ->
                            ( model
                                |> setUserData userData_
                                |> (updateChartable chartableId <| \c -> { c | colour = UserData.getChartableColour userData_ chartable_ })
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))
                            , Task.perform UserDataUpdated <| Task.succeed userData_
                            )

                        _ ->
                            ( model, Cmd.none )

                Chart.Chartable.ChartableInvertedChanged chartableId inverted ->
                    let
                        userData_ =
                            model.userData |> UserData.updateChartable chartableId (Chartable.setInverted inverted)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChartable chartableId <| \c -> { c | inverted = inverted })
                        |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableDeleteClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.deleteData i)

                        model_ =
                            model
                                |> setUserData userData_
                                |> (updateChart <| Chart.selectDataSet Nothing)
                                |> (\m ->
                                        { m
                                            | data = m.data |> List.filter (\( cId, _ ) -> cId /= ChartableId chartableId)
                                            , addState = NotAdding
                                        }
                                   )
                                |> updateChartableOptions userData_ model.chartId

                        ( model__, cmd ) =
                            model_
                                |> (updateChart1 <| Chart.removeDataSet (ChartableId chartableId))
                    in
                    ( model__
                    , Cmd.batch
                        [ Cmd.map ChartMsg cmd
                        , Task.perform UserDataUpdated <| Task.succeed userData_
                        ]
                    )

                Chart.Chartable.TrackableChanged chartableId trackableId (Just newTrackableId) ->
                    let
                        trackableOptionM =
                            model.data
                                |> Listx.lookup (ChartableId chartableId)
                                |> Maybe.andThen
                                    (\d ->
                                        case d of
                                            Chartable c ->
                                                Just c.trackableOptions

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.andThen (Listx.lookup newTrackableId)

                        userData_ =
                            model.userData |> UserData.updateChartable chartableId (Chartable.replaceTrackable trackableId newTrackableId)

                        chartableM_ =
                            userData_ |> UserData.getChartable chartableId
                    in
                    case ( trackableOptionM, chartableM_ ) of
                        ( Just ( question, _ ), Just chartable_ ) ->
                            ( model
                                |> setUserData userData_
                                |> (updateChartable chartableId <|
                                        \c ->
                                            { c
                                                | trackables = c.trackables |> Listx.updateLookupWithKey trackableId (\( _, t ) -> ( newTrackableId, { t | question = question } ))
                                                , colour = UserData.getChartableColour userData_ chartable_
                                                , trackableOptions = Chart.Chartable.buildTrackableOptions userData_ chartableId
                                            }
                                   )
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))
                            , Task.perform UserDataUpdated <| Task.succeed userData_
                            )

                        _ ->
                            ( model, Cmd.none )

                Chart.Chartable.TrackableMultiplierUpdated chartableId trackableId stringValue ->
                    let
                        multiplierM =
                            String.toFloat stringValue
                                |> Maybe.andThen
                                    (\v ->
                                        if v > 0 then
                                            Just v

                                        else
                                            Nothing
                                    )

                        userDataM_ =
                            multiplierM |> Maybe.map (\multiplier -> model.userData |> UserData.updateChartable chartableId (Chartable.setMultiplier trackableId multiplier))

                        userData_ =
                            Maybe.withDefault model.userData userDataM_
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChartable chartableId <|
                                \c ->
                                    { c
                                        | trackables =
                                            c.trackables
                                                |> Listx.updateLookup trackableId (\t -> { t | multiplier = stringValue, isValid = multiplierM /= Nothing })
                                    }
                           )
                        |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                    , case userDataM_ of
                        Just _ ->
                            Task.perform UserDataUpdated <| Task.succeed userData_

                        _ ->
                            Cmd.none
                    )

                Chart.Chartable.TrackableAddClicked chartableId ->
                    let
                        chartableM =
                            UserData.getChartable chartableId model.userData

                        trackableM =
                            chartableM
                                |> Maybe.map (List.map Tuple.first << .sum)
                                |> Maybe.map
                                    (\tIds ->
                                        model.data
                                            |> Listx.lookup (ChartableId chartableId)
                                            |> Maybe.andThen
                                                (\d ->
                                                    case d of
                                                        Chartable c ->
                                                            Just c.trackableOptions

                                                        _ ->
                                                            Nothing
                                                )
                                            |> Maybe.withDefault []
                                            |> List.filter (\( tId, ( _, visible ) ) -> visible && not (List.member tId tIds))
                                            |> List.map Tuple.first
                                    )
                                |> Maybe.andThen List.head
                                |> Maybe.map (\tId -> ( tId, 1.0 ))

                        trackableModelM =
                            trackableM |> Maybe.andThen (Chart.Chartable.toTrackableModel model.userData)

                        userDataM_ =
                            trackableM |> (Maybe.map <| \trackable -> model.userData |> UserData.updateChartable chartableId (Chartable.addTrackable trackable))

                        chartableM_ =
                            userDataM_ |> Maybe.andThen (UserData.getChartable chartableId)
                    in
                    case ( trackableModelM, chartableM_, userDataM_ ) of
                        ( Just ( trackableId, trackableModel ), Just chartable_, Just userData_ ) ->
                            ( model
                                |> setUserData userData_
                                |> (updateChartable chartableId <|
                                        \c ->
                                            { c
                                                | trackables = c.trackables |> Listx.insertLookup trackableId trackableModel
                                                , colour = UserData.getChartableColour userData_ chartable_
                                                , trackableOptions = Chart.Chartable.buildTrackableOptions userData_ chartableId
                                            }
                                   )
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))
                            , Task.perform UserDataUpdated <| Task.succeed userData_
                            )

                        _ ->
                            ( model, Cmd.none )

                Chart.Chartable.TrackableDeleteClicked chartableId trackableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateChartable chartableId (Chartable.deleteTrackable trackableId)

                        chartableM_ =
                            userData_ |> UserData.getChartable chartableId
                    in
                    case chartableM_ of
                        Just chartable_ ->
                            ( model
                                |> setUserData userData_
                                |> (updateChartable chartableId <|
                                        \c ->
                                            { c
                                                | trackables = c.trackables |> (List.filter <| \( tId, _ ) -> tId /= trackableId)
                                                , colour = UserData.getChartableColour userData_ chartable_
                                                , trackableOptions = Chart.Chartable.buildTrackableOptions userData_ chartableId
                                            }
                                   )
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))
                            , Task.perform UserDataUpdated <| Task.succeed userData_
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TrackableMsg i trackableMsg ->
            case trackableMsg of
                Chart.Trackable.TrackableHovered trackableId ->
                    ( model |> (updateChart <| Chart.hoverDataSet (trackableId |> Maybe.map TrackableId)), Cmd.none )

                Chart.Trackable.TrackableEditClicked trackableId ->
                    ( model |> (updateChart <| Chart.selectDataSet (Just (TrackableId trackableId)))
                    , Dom.focus ("chartable" ++ TrackableId.toString trackableId ++ "-name")
                        |> Task.attempt (always NoOp)
                    )

                Chart.Trackable.TrackableCloseClicked ->
                    ( model
                        |> (updateChart <| \c -> { c | expandedValue = False })
                        |> (updateChart <| Chart.selectDataSet Nothing)
                    , Cmd.none
                    )

                Chart.Trackable.TrackableVisibleClicked trackableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.toggleDataVisible i)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.toggleDataSetVisible (TrackableId trackableId))
                        |> (updateTrackable trackableId <| \c -> { c | visible = not c.visible })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Trackable.TrackableUpClicked trackableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataUp i)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.moveDataSetBack (TrackableId trackableId))
                        |> (\m -> { m | data = m.data |> Listx.moveHeadwardsBy Tuple.first (TrackableId trackableId) })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Trackable.TrackableDownClicked trackableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataDown i)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.moveDataSetForward (TrackableId trackableId))
                        |> (\m -> { m | data = m.data |> Listx.moveTailwardsBy Tuple.first (TrackableId trackableId) })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Trackable.TrackableInvertedChanged trackableId inverted ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.setTrackableInverted i inverted)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateTrackable trackableId <| \c -> { c | inverted = inverted })
                        |> (updateChart <| Chart.updateTrackableData userData_ trackableId Nothing (Just inverted))
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Trackable.TrackableMultiplierUpdated trackableId stringValue ->
                    let
                        multiplierM =
                            String.toFloat stringValue
                                |> Maybe.andThen
                                    (\v ->
                                        if v > 0 then
                                            Just v

                                        else
                                            Nothing
                                    )

                        userDataM_ =
                            multiplierM |> Maybe.map (\multiplier -> model.userData |> UserData.updateLineChart model.chartId (LineChart.setTrackableMultiplier i multiplier))

                        userData_ =
                            Maybe.withDefault model.userData userDataM_
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateTrackable trackableId <|
                                \t ->
                                    { t | multiplier = stringValue, isValid = multiplierM /= Nothing }
                           )
                        |> (updateChart <| Chart.updateTrackableData userData_ trackableId multiplierM Nothing)
                    , case userDataM_ of
                        Just _ ->
                            Task.perform UserDataUpdated <| Task.succeed userData_

                        _ ->
                            Cmd.none
                    )

                Chart.Trackable.TrackableChanged (Just trackableId) ->
                    case ( model.userData |> UserData.getLineChart model.chartId |> Maybe.map .data |> Maybe.andThen (Array.get i), model.userData |> UserData.getTrackable trackableId ) of
                        ( Just ( LineChart.Trackable { multiplier, inverted }, visible ), Just trackable ) ->
                            let
                                trackableOptions =
                                    buildTrackableOptions model.userData model.chartId

                                newTrackableModel =
                                    Chart.Trackable.init trackableOptions True trackable multiplier inverted visible

                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.replaceTrackable i trackableId multiplier inverted)

                                model_ =
                                    model
                                        |> setUserData userData_
                                        |> (\m ->
                                                { m
                                                    | data = List.take i m.data ++ ( TrackableId trackableId, Trackable newTrackableModel ) :: List.drop (i + 1) m.data
                                                    , addState = NotAdding
                                                }
                                           )
                                        |> updateChartableOptions userData_ model.chartId
                                        |> updateTrackableOptions userData_ model.chartId

                                ( model__, cmd ) =
                                    model_ |> (updateChart1 <| Chart.replaceDataSetWithTrackable userData_ i trackableId multiplier inverted visible)

                                model___ =
                                    model__
                                        |> (updateChart <| Chart.selectDataSet (Just (TrackableId trackableId)))
                            in
                            ( model___
                            , Cmd.batch
                                [ Cmd.map ChartMsg cmd
                                , Task.perform UserDataUpdated <| Task.succeed userData_
                                ]
                            )

                        _ ->
                            ( model, Cmd.none )

                -- in
                -- case ( trackableOptionM, chartableM_ ) of
                --     ( Just ( question, _ ), Just chartable_ ) ->
                --         ( model
                --             |> setUserData userData_
                --             |> (updateChartable chartableId <|
                --                     \c ->
                --                         { c
                --                             | trackables = c.trackables |> Listx.updateLookupWithKey trackableId (\( _, t ) -> ( newTrackableId, { t | question = question } ))
                --                             , colour = UserData.getChartableColour userData_ chartable_
                --                             , trackableOptions = Chart.Chartable.buildTrackableOptions userData_ chartableId
                --                         }
                --                )
                --             |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                --             |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))
                --         , Task.perform UserDataUpdated <| Task.succeed userData_
                --         )
                --     _ ->
                --         ( model, Cmd.none )
                Chart.Trackable.TrackableDeleteClicked trackableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.deleteData i)

                        model_ =
                            model
                                |> setUserData userData_
                                |> (updateChart <| Chart.selectDataSet Nothing)
                                |> (\m ->
                                        { m
                                            | data = m.data |> List.filter (\( cId, _ ) -> cId /= TrackableId trackableId)
                                            , addState = NotAdding
                                        }
                                   )
                                |> updateChartableOptions userData_ model.chartId
                                |> updateTrackableOptions userData_ model.chartId

                        ( model__, cmd ) =
                            model_
                                |> (updateChart1 <| Chart.removeDataSet (TrackableId trackableId))
                    in
                    ( model__
                    , Cmd.batch
                        [ Cmd.map ChartMsg cmd
                        , Task.perform UserDataUpdated <| Task.succeed userData_
                        ]
                    )

                Chart.Trackable.TrackableAddClicked trackableId ->
                    case model.userData |> UserData.getLineChart model.chartId |> Maybe.andThen (\c -> c.data |> Array.get i) of
                        Just ( LineChart.Trackable { inverted, multiplier }, _ ) ->
                            let
                                nextTrackableOption =
                                    model.chartableOptions
                                        |> List.filterMap
                                            (\( ( dId, visible ), _ ) ->
                                                case dId of
                                                    TrackableOption tId ->
                                                        if visible && tId /= trackableId then
                                                            Just tId

                                                        else
                                                            Nothing

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.head

                                newTrackable =
                                    case nextTrackableOption of
                                        Just tId ->
                                            [ ( tId, 1.0 ) ]

                                        _ ->
                                            []

                                chartableNames =
                                    model.chartableOptions
                                        |> List.filterMap
                                            (\( ( o, _ ), n ) ->
                                                case o of
                                                    ChartableOption _ ->
                                                        Just n

                                                    _ ->
                                                        Nothing
                                            )

                                newChartable =
                                    Debug.log "newChartable"
                                        { name = Stringx.nextName chartableNames "Chartable " 1
                                        , colour = Nothing
                                        , inverted = inverted
                                        , sum = ( trackableId, multiplier ) :: newTrackable
                                        }

                                ( chartableIdM, userData_ ) =
                                    Debug.log "userData_" (model.userData |> UserData.addChartable newChartable)
                            in
                            case chartableIdM of
                                Just chartableId ->
                                    let
                                        newChartableModel =
                                            Chart.Chartable.init userData_ True chartableId ( newChartable, True )

                                        userData__ =
                                            userData_ |> UserData.updateLineChart model.chartId (LineChart.replaceTrackableWithChartable i chartableId)

                                        model_ =
                                            model
                                                |> setUserData userData__
                                                |> (\m ->
                                                        { m
                                                            | data = List.take i m.data ++ ( ChartableId chartableId, Chartable newChartableModel ) :: List.drop (i + 1) m.data
                                                            , addState = NotAdding
                                                        }
                                                   )
                                                |> updateChartableOptions userData__ model.chartId
                                                |> updateTrackableOptions userData__ model.chartId

                                        ( model__, cmd ) =
                                            model_ |> (updateChart1 <| Chart.replaceDataSetWithChartable userData__ i chartableId)

                                        model___ =
                                            model__
                                                |> (updateChart <| Chart.selectDataSet (Just (ChartableId chartableId)))
                                    in
                                    ( model___
                                    , Cmd.batch
                                        [ Cmd.map ChartMsg cmd
                                        , Task.perform UserDataUpdated <| Task.succeed userData__
                                        ]
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChartableAddClicked ->
            ( model
                |> (updateChart <| Chart.selectDataSet Nothing)
                |> (\m ->
                        { m
                            | addState =
                                AddingChartable
                                    (model.chartableOptions
                                        |> List.filter (Tuple.second << Tuple.first)
                                        |> List.map (Tuple.first << Tuple.first)
                                        |> List.head
                                    )
                        }
                   )
            , Cmd.none
            )

        ChartableToAddChanged chartableId ->
            ( model
                |> (updateChart <| Chart.selectDataSet Nothing)
                |> (\m -> { m | addState = AddingChartable chartableId })
            , Cmd.none
            )

        ChartableAddConfirmClicked ->
            case model.addState of
                AddingChartable (Just (ChartableOption chartableId)) ->
                    case model.userData |> UserData.getChartable chartableId of
                        Just chartable ->
                            let
                                newChartableModel =
                                    Chart.Chartable.init userData_ True chartableId ( chartable, True )

                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.addChartable chartableId)

                                model_ =
                                    model
                                        |> setUserData userData_
                                        |> (\m ->
                                                { m
                                                    | data = m.data |> Listx.insertLookup (ChartableId chartableId) (Chartable newChartableModel)
                                                    , addState = NotAdding
                                                }
                                           )
                                        |> (updateChart <| Chart.selectDataSet Nothing)
                                        |> updateChartableOptions userData_ model.chartId

                                ( model__, cmd ) =
                                    model_ |> (updateChart1 <| Chart.addChartableDataSet userData_ chartableId)
                            in
                            ( model__
                            , Cmd.batch
                                [ Cmd.map ChartMsg cmd
                                , Task.perform UserDataUpdated <| Task.succeed userData_
                                ]
                            )

                        _ ->
                            ( model, Cmd.none )

                AddingChartable (Just (TrackableOption trackableId)) ->
                    case model.userData |> UserData.getTrackable trackableId of
                        Just trackable ->
                            let
                                trackableOptions =
                                    buildTrackableOptions model.userData model.chartId

                                newTrackableModel =
                                    Chart.Trackable.init trackableOptions True trackable 1 False True

                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.addTrackable trackableId)

                                model_ =
                                    model
                                        |> setUserData userData_
                                        |> (\m ->
                                                { m
                                                    | data = m.data |> Listx.insertLookup (TrackableId trackableId) (Trackable newTrackableModel)
                                                    , addState = NotAdding
                                                }
                                           )
                                        |> (updateChart <| Chart.selectDataSet Nothing)
                                        |> updateChartableOptions userData_ model.chartId
                                        |> updateTrackableOptions userData_ model.chartId

                                ( model__, cmd ) =
                                    model_ |> (updateChart1 <| Chart.addTrackableDataSet userData_ trackableId)
                            in
                            ( model__
                            , Cmd.batch
                                [ Cmd.map ChartMsg cmd
                                , Task.perform UserDataUpdated <| Task.succeed userData_
                                ]
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChartableAddCancelClicked ->
            ( model
                |> (updateChart <| Chart.selectDataSet Nothing)
                |> (\m -> { m | addState = NotAdding })
            , Cmd.none
            )

        ChartMsg chartMsg ->
            let
                ( chart_, cmd ) =
                    Chart.update chartMsg model.chart
            in
            ( model |> updateChart (always chart_), Cmd.map ChartMsg cmd )

        MoveDataClicked ->
            let
                userData_ =
                    model.userData |> UserData.moveData model.chart.graph.today
            in
            ( model |> setUserData userData_
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "bg-white" ]
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text <| Stringx.withDefault "Chart" model.chart.name ]
        , div
            []
            [ Html.map ChartMsg (Chart.view model.chart)
            , div [ class "mt-8 px-4 py-2 bg-gray-300 border-t-4 border-gray-400" ]
                [ Controls.textbox [ class "" ]
                    [ id <| "chart-name"
                    , placeholder "Name"
                    ]
                    model.chart.name
                    { isValid = True, isRequired = True, isPristine = model.nameIsPristine }
                    ChartNameUpdated
                ]
            , div [ class "bg-gray-200" ] <|
                (model.data
                    |> List.indexedMap
                        (\i d ->
                            case d of
                                ( ChartableId cId, Chartable c ) ->
                                    Chart.Chartable.view
                                        ((Maybe.map Tuple.first << List.head) model.chart.graph.data)
                                        ((Maybe.map Tuple.first << List.head << List.reverse) model.chart.graph.data)
                                        model.chart.graph.selectedDataSet
                                        ( cId, c )
                                        |> List.map (Html.map (ChartableMsg i))

                                ( TrackableId tId, Trackable t ) ->
                                    Chart.Trackable.view
                                        ((Maybe.map Tuple.first << List.head) model.chart.graph.data)
                                        ((Maybe.map Tuple.first << List.head << List.reverse) model.chart.graph.data)
                                        model.chart.graph.selectedDataSet
                                        ( tId, t )
                                        |> List.map (Html.map (TrackableMsg i))

                                _ ->
                                    []
                        )
                    |> List.concat
                )
                    ++ [ case model.addState of
                            AddingChartable addingChartableId ->
                                let
                                    toString option =
                                        case option of
                                            ChartableOption id ->
                                                "c" ++ ChartableId.toString id

                                            TrackableOption id ->
                                                "t" ++ TrackableId.toString id

                                    fromString str =
                                        case String.toList str of
                                            'c' :: cs ->
                                                Maybe.map ChartableOption <| ChartableId.fromString <| String.fromList cs

                                            't' :: cs ->
                                                Maybe.map TrackableOption <| TrackableId.fromString <| String.fromList cs

                                            _ ->
                                                Nothing
                                in
                                div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                                    [ Controls.textDropdown "w-full h-10"
                                        ChartableToAddChanged
                                        toString
                                        fromString
                                        model.chartableOptions
                                        Nothing
                                        addingChartableId
                                        { showFilled = False }
                                    , Controls.button "ml-4" Controls.ButtonGrey ChartableAddConfirmClicked SolidPlusCircle "Add" True
                                    , button
                                        [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , Htmlx.onClickStopPropagation ChartableAddCancelClicked
                                        ]
                                        [ icon "w-5 h-5" <| SolidTimes ]
                                    ]

                            _ ->
                                div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                                    [ Controls.button "" Controls.ButtonGrey ChartableAddClicked SolidPlusCircle "Add data" (model.chartableOptions |> List.any (Tuple.second << Tuple.first))

                                    -- , Controls.button "ml-4" Controls.ButtonGrey MoveDataClicked SolidPlusCircle "Move data" True
                                    ]
                       ]
            ]
        ]
