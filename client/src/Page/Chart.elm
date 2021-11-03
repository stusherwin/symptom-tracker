module Page.Chart exposing (Model, Msg(..), init, subscriptions, update, view)

import Array
import Browser.Dom as Dom
import Chart.Chartable
import Chart.LineChart as Chart exposing (DataSetId(..))
import Chart.Trackable
import Controls
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Htmlx
import Listx
import Maybe exposing (Maybe)
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
    }


type DataOption
    = ChartableOption ChartableId
    | TrackableOption TrackableId


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
                                    |> Maybe.map (\t -> ( TrackableId id, Trackable <| Chart.Trackable.init trackableOptions True id t (String.fromFloat multiplier) inverted visible ))
                    )
      , nameIsPristine = True
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
        updateChart : (Chart.Model -> ( Chart.Model, Cmd Chart.Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
        updateChart fn ( m, cmd ) =
            let
                ( m_, cmd_ ) =
                    m.chart |> fn
            in
            ( { m | chart = m_ }, Cmd.batch [ cmd, Cmd.map ChartMsg cmd_ ] )

        updateTrackableOptions trackableOptions ( m, cmd ) =
            ( { m
                | data =
                    m.data
                        |> List.map
                            (Tuple.mapSecond
                                (\d ->
                                    case d of
                                        Trackable t ->
                                            Trackable { t | options = trackableOptions }

                                        _ ->
                                            d
                                )
                            )
              }
            , cmd
            )

        updateChartableOptions userData chartId ( m, cmd ) =
            ( { m | chartableOptions = buildChartableOptions userData chartId }
            , cmd
            )

        setUserData userData_ ( m, cmd ) =
            ( { m | userData = userData_ }
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )
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
            ( model, Cmd.none )
                |> setUserData userData_
                |> (Tuple.mapFirst <| \m -> { m | chart = { chart | name = name }, nameIsPristine = False })

        ChartableMsg i chartableMsg ->
            case model.data |> Array.fromList |> Array.get i of
                Just ( ChartableId chartableId, Chartable chartable ) ->
                    let
                        updateChartable : (Chart.Chartable.Model -> ( Chart.Chartable.Model, Cmd Chart.Chartable.Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                        updateChartable fn ( m, c ) =
                            let
                                ( chartable_, c_ ) =
                                    fn chartable
                            in
                            ( { m | data = m.data |> Array.fromList |> Array.set i ( ChartableId chartableId, Chartable chartable_ ) |> Array.toList }
                            , Cmd.batch [ c, Cmd.map (ChartableMsg i) c_ ]
                            )
                    in
                    case chartableMsg of
                        Chart.Chartable.ChartableHovered True ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update model.userData chartableMsg)
                                |> (updateChart <| Chart.hoverDataSet (Just <| ChartableId chartableId))

                        Chart.Chartable.ChartableHovered False ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update model.userData chartableMsg)
                                |> (updateChart <| Chart.hoverDataSet Nothing)

                        Chart.Chartable.ChartableEditClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update model.userData chartableMsg)
                                |> (updateChart <| Chart.selectDataSet (Just (ChartableId chartableId)))

                        Chart.Chartable.ChartableCloseClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update model.userData chartableMsg)
                                |> (updateChart <| \c -> ( { c | expandedValue = False }, Cmd.none ))
                                |> (updateChart <| Chart.selectDataSet Nothing)

                        Chart.Chartable.ChartableVisibleClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.toggleDataVisible i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.toggleDataSetVisible (ChartableId chartableId))

                        Chart.Chartable.ChartableUpClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataUp i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.moveDataSetBack (ChartableId chartableId))
                                |> (Tuple.mapFirst <| \m -> { m | data = m.data |> Listx.moveHeadwardsBy Tuple.first (ChartableId chartableId) })

                        Chart.Chartable.ChartableDownClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataDown i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.moveDataSetForward (ChartableId chartableId))
                                |> (Tuple.mapFirst <| \m -> { m | data = m.data |> Listx.moveTailwardsBy Tuple.first (ChartableId chartableId) })

                        Chart.Chartable.ChartableNameUpdated name ->
                            let
                                userData_ =
                                    if not <| String.isEmpty name then
                                        model.userData |> UserData.updateChartable chartableId (Chartable.setName name)

                                    else
                                        model.userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> updateChartableOptions userData_ model.chartId
                                |> (updateChart <| Chart.updateDataSetName (ChartableId chartableId) name)

                        Chart.Chartable.ChartableColourUpdated colour ->
                            let
                                userData_ =
                                    model.userData
                                        |> UserData.updateChartable chartableId (Chartable.setColour colour)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))

                        Chart.Chartable.ChartableInvertedChanged inverted ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.setInverted inverted)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)

                        Chart.Chartable.ChartableDeleteClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.deleteData i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.removeDataSet (ChartableId chartableId))
                                |> (updateChart <| Chart.selectDataSet Nothing)
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | data = m.data |> List.filter (\( cId, _ ) -> cId /= ChartableId chartableId)
                                                , addState = NotAdding
                                            }
                                   )
                                |> updateChartableOptions userData_ model.chartId

                        Chart.Chartable.TrackableChanged trackableId (Just newTrackableId) ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.replaceTrackable trackableId newTrackableId)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))

                        Chart.Chartable.TrackableMultiplierUpdated trackableId stringValue ->
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

                                userData_ =
                                    case multiplierM of
                                        Just multiplier ->
                                            model.userData |> UserData.updateChartable chartableId (Chartable.setMultiplier trackableId multiplier)

                                        _ ->
                                            model.userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)

                        Chart.Chartable.TrackableAddClicked (Just trackableId) ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.addTrackable ( trackableId, 1.0 ))
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))

                        Chart.Chartable.TrackableDeleteClicked trackableId ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.deleteTrackable trackableId)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableData userData_ chartableId)
                                |> (updateChart <| Chart.updateDataSetColour userData_ (ChartableId chartableId))

                        _ ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update model.userData chartableMsg)

                _ ->
                    ( model, Cmd.none )

        TrackableMsg i trackableMsg ->
            case model.data |> Array.fromList |> Array.get i of
                Just ( TrackableId trackableId, Trackable trackable ) ->
                    let
                        updateTrackable : (Chart.Trackable.Model -> ( Chart.Trackable.Model, Cmd Chart.Trackable.Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                        updateTrackable fn ( m, c ) =
                            let
                                ( trackable_, c_ ) =
                                    fn trackable
                            in
                            ( { m | data = m.data |> Array.fromList |> Array.set i ( TrackableId trackableId, Trackable trackable_ ) |> Array.toList }
                            , Cmd.batch [ c, Cmd.map (TrackableMsg i) c_ ]
                            )
                    in
                    case trackableMsg of
                        Chart.Trackable.TrackableHovered True ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update model.userData Nothing trackableMsg)
                                |> (updateChart <| Chart.hoverDataSet <| Just <| TrackableId trackableId)

                        Chart.Trackable.TrackableHovered False ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update model.userData Nothing trackableMsg)
                                |> (updateChart <| Chart.hoverDataSet Nothing)

                        Chart.Trackable.TrackableEditClicked ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update model.userData Nothing trackableMsg)
                                |> (updateChart <| Chart.selectDataSet (Just (TrackableId trackableId)))

                        Chart.Trackable.TrackableCloseClicked ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update model.userData Nothing trackableMsg)
                                |> (updateChart <| \c -> ( { c | expandedValue = False }, Cmd.none ))
                                |> (updateChart <| Chart.selectDataSet Nothing)

                        Chart.Trackable.TrackableVisibleClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.toggleDataVisible i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.toggleDataSetVisible (TrackableId trackableId))

                        Chart.Trackable.TrackableUpClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataUp i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.moveDataSetBack (TrackableId trackableId))
                                |> (Tuple.mapFirst <| \m -> { m | data = m.data |> Listx.moveHeadwardsBy Tuple.first (TrackableId trackableId) })

                        Chart.Trackable.TrackableDownClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.moveDataDown i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.moveDataSetForward (TrackableId trackableId))
                                |> (Tuple.mapFirst <| \m -> { m | data = m.data |> Listx.moveTailwardsBy Tuple.first (TrackableId trackableId) })

                        Chart.Trackable.TrackableInvertedChanged inverted ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.setTrackableInverted i inverted)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.updateTrackableData userData_ trackableId Nothing (Just inverted))

                        Chart.Trackable.TrackableMultiplierUpdated stringValue ->
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

                                userData_ =
                                    case multiplierM of
                                        Just multiplier ->
                                            model.userData |> UserData.updateLineChart model.chartId (LineChart.setTrackableMultiplier i multiplier)

                                        _ ->
                                            model.userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.updateTrackableData userData_ trackableId multiplierM Nothing)

                        Chart.Trackable.TrackableChanged (Just newTrackableId) ->
                            case model.userData |> UserData.getLineChart model.chartId |> Maybe.map .data |> Maybe.andThen (Array.get i) of
                                Just ( LineChart.Trackable { multiplier, inverted }, visible ) ->
                                    let
                                        userData_ =
                                            model.userData |> UserData.updateLineChart model.chartId (LineChart.replaceTrackable i newTrackableId multiplier inverted)

                                        trackableOptions =
                                            buildTrackableOptions userData_ model.chartId
                                    in
                                    ( model, Cmd.none )
                                        |> setUserData userData_
                                        |> (updateTrackable <| Chart.Trackable.update userData_ (Just trackableOptions) trackableMsg)
                                        |> (Tuple.mapFirst <| \m -> { m | addState = NotAdding })
                                        |> updateChartableOptions userData_ model.chartId
                                        |> updateTrackableOptions trackableOptions
                                        |> (updateChart <| Chart.replaceDataSetWithTrackable userData_ i newTrackableId multiplier inverted visible)
                                        |> (updateChart <| Chart.selectDataSet (Just (TrackableId newTrackableId)))

                                _ ->
                                    ( model, Cmd.none )

                        Chart.Trackable.TrackableDeleteClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.deleteData i)

                                trackableOptions =
                                    buildTrackableOptions userData_ model.chartId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ (Just trackableOptions) trackableMsg)
                                |> (updateChart <| Chart.selectDataSet Nothing)
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | data = m.data |> List.filter (\( cId, _ ) -> cId /= TrackableId trackableId)
                                                , addState = NotAdding
                                            }
                                   )
                                |> updateChartableOptions userData_ model.chartId
                                |> updateTrackableOptions trackableOptions
                                |> (updateChart <| Chart.removeDataSet (TrackableId trackableId))

                        Chart.Trackable.TrackableAddClicked ->
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

                                                trackableOptions =
                                                    buildTrackableOptions userData__ model.chartId
                                            in
                                            ( model, Cmd.none )
                                                |> setUserData userData__
                                                |> (updateTrackable <| Chart.Trackable.update userData__ (Just trackableOptions) trackableMsg)
                                                |> (Tuple.mapFirst <|
                                                        \m ->
                                                            { m
                                                                | data = List.take i m.data ++ ( ChartableId chartableId, Chartable newChartableModel ) :: List.drop (i + 1) m.data
                                                                , addState = NotAdding
                                                            }
                                                   )
                                                |> updateChartableOptions userData__ model.chartId
                                                |> updateTrackableOptions trackableOptions
                                                |> (updateChart <| Chart.replaceDataSetWithChartable userData__ i chartableId)
                                                |> (updateChart <| Chart.selectDataSet (Just (ChartableId chartableId)))

                                        _ ->
                                            ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )
                                        |> (updateTrackable <| Chart.Trackable.update model.userData Nothing trackableMsg)

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChartableAddClicked ->
            ( model, Cmd.none )
                |> (updateChart <| Chart.selectDataSet Nothing)
                |> (Tuple.mapFirst <|
                        \m ->
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

        ChartableToAddChanged chartableId ->
            ( model, Cmd.none )
                |> (updateChart <| Chart.selectDataSet Nothing)
                |> (Tuple.mapFirst <| \m -> { m | addState = AddingChartable chartableId })

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
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | data = m.data |> Listx.insertLookup (ChartableId chartableId) (Chartable newChartableModel)
                                                , addState = NotAdding
                                            }
                                   )
                                |> updateChartableOptions userData_ model.chartId
                                |> (updateChart <| Chart.addChartableDataSet userData_ chartableId)
                                |> (updateChart <| Chart.selectDataSet Nothing)

                        _ ->
                            ( model, Cmd.none )

                AddingChartable (Just (TrackableOption trackableId)) ->
                    case model.userData |> UserData.getTrackable trackableId of
                        Just trackable ->
                            let
                                newTrackableModel =
                                    Chart.Trackable.init trackableOptions True trackableId trackable "1" False True

                                userData_ =
                                    model.userData |> UserData.updateLineChart model.chartId (LineChart.addTrackable trackableId)

                                trackableOptions =
                                    buildTrackableOptions userData_ model.chartId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | data = m.data |> Listx.insertLookup (TrackableId trackableId) (Trackable newTrackableModel)
                                                , addState = NotAdding
                                            }
                                   )
                                |> updateChartableOptions userData_ model.chartId
                                |> updateTrackableOptions trackableOptions
                                |> (updateChart <| Chart.selectDataSet Nothing)
                                |> (updateChart <| Chart.addTrackableDataSet userData_ trackableId)

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChartableAddCancelClicked ->
            ( model, Cmd.none )
                |> (updateChart <| Chart.selectDataSet Nothing)
                |> (Tuple.mapFirst <| \m -> { m | addState = NotAdding })

        ChartMsg chartMsg ->
            ( model, Cmd.none )
                |> updateChart (Chart.update chartMsg)

        MoveDataClicked ->
            let
                userData_ =
                    model.userData |> UserData.moveData model.chart.graph.today
            in
            ( model, Cmd.none )
                |> setUserData userData_

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        dataCount =
            List.length model.data
    in
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
                                        { canMoveUp = i > 0
                                        , canMoveDown = i < dataCount - 1
                                        , isSelected = model.chart.graph.selectedDataSet == Just (ChartableId cId)
                                        , isAnySelected = model.chart.graph.selectedDataSet /= Nothing
                                        }
                                        c
                                        |> List.map (Html.map (ChartableMsg i))

                                ( TrackableId tId, Trackable t ) ->
                                    Chart.Trackable.view
                                        { canMoveUp = i > 0
                                        , canMoveDown = i < dataCount - 1
                                        , isSelected = model.chart.graph.selectedDataSet == Just (TrackableId tId)
                                        , isAnySelected = model.chart.graph.selectedDataSet /= Nothing
                                        }
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
