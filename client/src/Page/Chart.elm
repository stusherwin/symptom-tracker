module Page.Chart exposing (Model, Msg(..), init, subscriptions, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Chart.Chartable
import Chart.LineChart as Chart
import Chart.Trackable
import Controls
import Date exposing (Date, Unit(..))
import Extra.Array as Array
import Extra.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (Maybe)
import Stringx
import Svg.Attributes exposing (in_)
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as C exposing (Chartable)
import UserData.ChartableId as CId exposing (ChartableId)
import UserData.LineChart as LC exposing (LineChart(..))
import UserData.LineChartId as LCId exposing (LineChartId)
import UserData.Trackable as T exposing (Responses(..))
import UserData.TrackableId as TId exposing (TrackableId)


type alias Model =
    { chartId : LineChartId
    , chart : Chart.Model
    , chartableOptions : List ( ( DataOption, Bool ), String )
    , addState : EditState
    , elements : Array ChartElement
    , nameIsPristine : Bool
    }


type DataOption
    = ChartableOption ChartableId
    | TrackableOption TrackableId


type ChartElement
    = ChartableElement Chart.Chartable.Model
    | TrackableElement Chart.Trackable.Model


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
      , elements =
            LC.dataSets chart
                |> Array.map
                    (\( data, visible ) ->
                        case data of
                            LC.ChartableElement { chartableId, chartable } ->
                                ChartableElement <| Chart.Chartable.init userData True chartableId ( chartable, visible )

                            LC.TrackableElement { trackableId, trackable, multiplier, isInverted } ->
                                TrackableElement <| Chart.Trackable.init trackableOptions True trackableId trackable (String.fromFloat multiplier) isInverted visible
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
                |> Maybe.map (Array.toList << LC.dataSets)
                |> Maybe.withDefault []
                |> List.map Tuple.first
                |> List.filterMap
                    (\dataSetId ->
                        case dataSetId of
                            LC.TrackableElement { trackableId } ->
                                Just trackableId

                            _ ->
                                Nothing
                    )

        chartablesInUse =
            UserData.getLineChart chartId userData
                |> Maybe.map (Array.toList << LC.dataSets)
                |> Maybe.withDefault []
                |> List.map Tuple.first
                |> List.filterMap
                    (\dataSetId ->
                        case dataSetId of
                            LC.ChartableElement { chartableId } ->
                                Just chartableId

                            _ ->
                                Nothing
                    )
    in
    (UserData.activeTrackables userData
        |> List.filter
            (\( _, ( t, _ ) ) ->
                case T.responses t of
                    TText _ ->
                        False

                    _ ->
                        True
            )
        |> (List.filter <| Tuple.second << Tuple.second)
        |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << T.question << Tuple.first))
        |> (List.map <| \( tId, name ) -> ( ( TrackableOption tId, not <| List.member tId trackablesInUse ), name ))
        |> (List.sortBy <| String.toUpper << Tuple.second)
    )
        ++ (UserData.activeChartables userData
                |> (List.filter <| Tuple.second << Tuple.second)
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << C.name << Tuple.first))
                |> (List.map <| \( cId, name ) -> ( ( ChartableOption cId, not <| List.member cId chartablesInUse ), name ))
                |> (List.sortBy <| String.toUpper << Tuple.second)
           )


buildTrackableOptions : UserData -> LineChartId -> List ( TrackableId, ( String, Bool ) )
buildTrackableOptions userData chartId =
    let
        trackablesInUse =
            UserData.getLineChart chartId userData
                |> Maybe.map (Array.toList << LC.dataSets)
                |> Maybe.withDefault []
                |> List.map Tuple.first
                |> List.filterMap
                    (\dataSetId ->
                        case dataSetId of
                            LC.TrackableElement { trackableId } ->
                                Just trackableId

                            _ ->
                                Nothing
                    )
    in
    UserData.activeTrackables userData
        |> List.filter
            (\( _, ( t, _ ) ) ->
                case T.responses t of
                    TText _ ->
                        False

                    _ ->
                        True
            )
        |> (List.filter <| Tuple.second << Tuple.second)
        |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << T.question << Tuple.first))
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


update : UserData -> Msg -> Model -> ( Model, Cmd Msg )
update userData msg model =
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
                | elements =
                    m.elements
                        |> Array.map
                            (\d ->
                                case d of
                                    TrackableElement t ->
                                        TrackableElement { t | options = trackableOptions }

                                    _ ->
                                        d
                            )
              }
            , cmd
            )

        updateChartableOptions userData_ chartId ( m, cmd ) =
            ( { m | chartableOptions = buildChartableOptions userData_ chartId }
            , cmd
            )

        setUserData userData_ ( m, cmd ) =
            ( m
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )
    in
    case msg of
        ChartNameUpdated name ->
            let
                chart =
                    model.chart

                userData_ =
                    userData
                        |> (if not <| String.isEmpty name then
                                UserData.updateLineChart model.chartId (LC.setName name)

                            else
                                identity
                           )
            in
            ( model, Cmd.none )
                |> setUserData userData_
                |> (Tuple.mapFirst <| \m -> { m | chart = { chart | name = name }, nameIsPristine = False })

        ChartableMsg i chartableMsg ->
            case model.elements |> Array.get i of
                Just (ChartableElement chartable) ->
                    let
                        updateChartable : (Chart.Chartable.Model -> ( Chart.Chartable.Model, Cmd Chart.Chartable.Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                        updateChartable fn ( m, c ) =
                            let
                                ( chartable_, c_ ) =
                                    fn chartable
                            in
                            ( { m | elements = m.elements |> Array.set i (ChartableElement chartable_) }
                            , Cmd.batch [ c, Cmd.map (ChartableMsg i) c_ ]
                            )
                    in
                    case chartableMsg of
                        Chart.Chartable.ChartableHovered True ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update userData chartableMsg)
                                |> (updateChart <| Chart.hoverDataSet (Just i))

                        Chart.Chartable.ChartableHovered False ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update userData chartableMsg)
                                |> (updateChart <| Chart.hoverDataSet Nothing)

                        Chart.Chartable.ChartableEditClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update userData chartableMsg)
                                |> (updateChart <| Chart.selectDataSet (Just i))

                        Chart.Chartable.ChartableCloseClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update userData chartableMsg)
                                |> (updateChart <| \c -> ( { c | expandedValue = False }, Cmd.none ))
                                |> (updateChart <| Chart.selectDataSet Nothing)

                        Chart.Chartable.ChartableVisibleClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.toggleDataVisible i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.toggleDataSetVisible i)

                        Chart.Chartable.ChartableUpClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.moveDataUp i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.moveDataSetBack i)
                                |> (Tuple.mapFirst <| \m -> { m | elements = m.elements |> Array.swap i (i - 1) })

                        Chart.Chartable.ChartableDownClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.moveDataDown i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.moveDataSetForward i)
                                |> (Tuple.mapFirst <| \m -> { m | elements = m.elements |> Array.swap i (i + 1) })

                        Chart.Chartable.ChartableNameUpdated name ->
                            let
                                userData_ =
                                    if not <| String.isEmpty name then
                                        userData |> UserData.updateChartable chartable.chartableId (C.setName name)

                                    else
                                        userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> updateChartableOptions userData_ model.chartId
                                |> (updateChart <| Chart.updateDataSetName i name)

                        Chart.Chartable.ChartableColourUpdated colour ->
                            let
                                userData_ =
                                    userData
                                        |> UserData.updateChartable chartable.chartableId (C.setOwnColour colour)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableDataSet i userData_ chartable.chartableId)

                        Chart.Chartable.ChartableInvertedChanged inverted ->
                            let
                                userData_ =
                                    userData |> UserData.updateChartable chartable.chartableId (C.setInverted inverted)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableDataSet i userData_ chartable.chartableId)

                        Chart.Chartable.ChartableDeleteClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.deleteData i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.removeDataSet i)
                                |> (updateChart <| Chart.selectDataSet Nothing)
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | elements = m.elements |> Array.delete i
                                                , addState = NotAdding
                                            }
                                   )
                                |> updateChartableOptions userData_ model.chartId

                        Chart.Chartable.TrackableChanged trackableId (Just newTrackableId) ->
                            let
                                newTrackableM =
                                    userData |> UserData.getTrackable newTrackableId

                                userData_ =
                                    case newTrackableM of
                                        Just newTrackable ->
                                            userData |> UserData.updateChartable chartable.chartableId (C.replaceTrackable trackableId newTrackableId newTrackable)

                                        _ ->
                                            userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableDataSet i userData_ chartable.chartableId)

                        -- |> (updateChart <| Chart.updateDataSetColour i userData_ (ChartableId chartable.chartableId))
                        Chart.Chartable.TrackableMultiplierUpdated trackableId stringValue ->
                            let
                                userData_ =
                                    case T.parseMultiplier stringValue of
                                        Just multiplier ->
                                            userData |> UserData.updateChartable chartable.chartableId (C.setMultiplier trackableId multiplier)

                                        _ ->
                                            userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableDataSet i userData_ chartable.chartableId)

                        Chart.Chartable.TrackableAddClicked (Just trackableId) ->
                            let
                                trackableM =
                                    userData |> UserData.getTrackable trackableId

                                userData_ =
                                    case trackableM of
                                        Just trackable ->
                                            userData |> UserData.updateChartable chartable.chartableId (C.addTrackable trackableId trackable 1.0)

                                        _ ->
                                            userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableDataSet i userData_ chartable.chartableId)

                        -- |> (updateChart <| Chart.updateDataSetColour i userData_ (ChartableId chartable.chartableId))
                        Chart.Chartable.TrackableDeleteClicked trackableId ->
                            let
                                userData_ =
                                    userData |> UserData.updateChartable chartable.chartableId (C.deleteTrackable trackableId)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (updateChart <| Chart.updateChartableDataSet i userData_ chartable.chartableId)

                        -- |> (updateChart <| Chart.updateDataSetColour i userData_ (ChartableId chartable.chartableId))
                        _ ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update userData chartableMsg)

                _ ->
                    ( model, Cmd.none )

        TrackableMsg i trackableMsg ->
            case model.elements |> Array.get i of
                Just (TrackableElement trackable) ->
                    let
                        updateTrackable : (Chart.Trackable.Model -> ( Chart.Trackable.Model, Cmd Chart.Trackable.Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                        updateTrackable fn ( m, c ) =
                            let
                                ( trackable_, c_ ) =
                                    fn trackable
                            in
                            ( { m | elements = m.elements |> Array.set i (TrackableElement trackable_) }
                            , Cmd.batch [ c, Cmd.map (TrackableMsg i) c_ ]
                            )
                    in
                    case trackableMsg of
                        Chart.Trackable.TrackableHovered True ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update userData Nothing trackableMsg)
                                |> (updateChart <| Chart.hoverDataSet (Just i))

                        Chart.Trackable.TrackableHovered False ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update userData Nothing trackableMsg)
                                |> (updateChart <| Chart.hoverDataSet Nothing)

                        Chart.Trackable.TrackableEditClicked ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update userData Nothing trackableMsg)
                                |> (updateChart <| Chart.selectDataSet (Just i))

                        Chart.Trackable.TrackableCloseClicked ->
                            ( model, Cmd.none )
                                |> (updateTrackable <| Chart.Trackable.update userData Nothing trackableMsg)
                                |> (updateChart <| \c -> ( { c | expandedValue = False }, Cmd.none ))
                                |> (updateChart <| Chart.selectDataSet Nothing)

                        Chart.Trackable.TrackableVisibleClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.toggleDataVisible i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.toggleDataSetVisible i)

                        Chart.Trackable.TrackableUpClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.moveDataUp i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.moveDataSetBack i)
                                |> (Tuple.mapFirst <| \m -> { m | elements = m.elements |> Array.swap i (i - 1) })

                        Chart.Trackable.TrackableDownClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.moveDataDown i)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.moveDataSetForward i)
                                |> (Tuple.mapFirst <| \m -> { m | elements = m.elements |> Array.swap i (i + 1) })

                        Chart.Trackable.TrackableInvertedChanged inverted ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.setTrackableInverted i inverted)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.updateTrackableDataSet i userData_ trackable.trackableId Nothing (Just inverted))

                        Chart.Trackable.TrackableMultiplierUpdated stringValue ->
                            let
                                multiplierM =
                                    T.parseMultiplier stringValue

                                userData_ =
                                    case multiplierM of
                                        Just multiplier ->
                                            userData |> UserData.updateLineChart model.chartId (LC.setTrackableMultiplier i multiplier)

                                        _ ->
                                            userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateTrackable <| Chart.Trackable.update userData_ Nothing trackableMsg)
                                |> (updateChart <| Chart.updateTrackableDataSet i userData_ trackable.trackableId multiplierM Nothing)

                        Chart.Trackable.TrackableChanged (Just newTrackableId) ->
                            let
                                oldTrackableStateM =
                                    userData |> UserData.getLineChart model.chartId |> Maybe.map LC.dataSets |> Maybe.andThen (Array.get i)

                                newTrackableM =
                                    userData |> UserData.getTrackable newTrackableId
                            in
                            case ( oldTrackableStateM, newTrackableM ) of
                                ( Just ( LC.TrackableElement { multiplier, isInverted }, visible ), Just newTrackable ) ->
                                    let
                                        userData_ =
                                            userData |> UserData.updateLineChart model.chartId (LC.replaceTrackable i newTrackableId newTrackable multiplier isInverted)

                                        trackableOptions =
                                            buildTrackableOptions userData_ model.chartId
                                    in
                                    ( model, Cmd.none )
                                        |> setUserData userData_
                                        |> (updateTrackable <| Chart.Trackable.update userData_ (Just trackableOptions) trackableMsg)
                                        |> (Tuple.mapFirst <| \m -> { m | addState = NotAdding })
                                        |> updateChartableOptions userData_ model.chartId
                                        |> updateTrackableOptions trackableOptions
                                        |> (updateChart <| Chart.replaceDataSetWithTrackable userData_ i newTrackableId multiplier isInverted visible)
                                        |> (updateChart <| Chart.selectDataSet (Just i))

                                _ ->
                                    ( model, Cmd.none )

                        Chart.Trackable.TrackableDeleteClicked ->
                            let
                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.deleteData i)

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
                                                | elements = m.elements |> Array.delete i
                                                , addState = NotAdding
                                            }
                                   )
                                |> updateChartableOptions userData_ model.chartId
                                |> updateTrackableOptions trackableOptions
                                |> (updateChart <| Chart.removeDataSet i)

                        Chart.Trackable.TrackableAddClicked ->
                            case userData |> UserData.getLineChart model.chartId |> Maybe.andThen (LC.dataSets >> Array.get i) of
                                Just ( LC.TrackableElement { isInverted, multiplier }, _ ) ->
                                    let
                                        nextTrackableOption =
                                            model.chartableOptions
                                                |> List.filterMap
                                                    (\( ( dId, visible ), _ ) ->
                                                        case dId of
                                                            TrackableOption tId ->
                                                                if visible && tId /= trackable.trackableId then
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
                                            , ownColour = Nothing
                                            , isInverted = isInverted
                                            , sum = ( trackable.trackableId, multiplier ) :: newTrackable
                                            }

                                        ( chartableIdM, userData_ ) =
                                            userData |> UserData.addChartable newChartable
                                    in
                                    case chartableIdM of
                                        Just ( chartableId, chartable ) ->
                                            let
                                                newChartableModel =
                                                    Chart.Chartable.init userData_ True chartableId ( chartable, True )

                                                userData__ =
                                                    userData_ |> UserData.updateLineChart model.chartId (LC.replaceTrackableWithChartable i chartableId chartable)

                                                trackableOptions =
                                                    buildTrackableOptions userData__ model.chartId
                                            in
                                            ( model, Cmd.none )
                                                |> setUserData userData__
                                                |> (updateTrackable <| Chart.Trackable.update userData__ (Just trackableOptions) trackableMsg)
                                                |> (Tuple.mapFirst <|
                                                        \m ->
                                                            { m
                                                                | elements = m.elements |> Array.set i (ChartableElement newChartableModel)
                                                                , addState = NotAdding
                                                            }
                                                   )
                                                |> updateChartableOptions userData__ model.chartId
                                                |> updateTrackableOptions trackableOptions
                                                |> (updateChart <| Chart.replaceDataSetWithChartable userData__ i chartableId)
                                                |> (updateChart <| Chart.selectDataSet (Just i))

                                        _ ->
                                            ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )
                                        |> (updateTrackable <| Chart.Trackable.update userData Nothing trackableMsg)

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
                    case userData |> UserData.getChartable chartableId of
                        Just chartable ->
                            let
                                newChartableModel =
                                    Chart.Chartable.init userData_ True chartableId ( chartable, True )

                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.addChartable chartableId chartable)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | elements = m.elements |> Array.push (ChartableElement newChartableModel)
                                                , addState = NotAdding
                                            }
                                   )
                                |> updateChartableOptions userData_ model.chartId
                                |> (updateChart <| Chart.addChartableDataSet userData_ chartableId)
                                |> (updateChart <| Chart.selectDataSet Nothing)

                        _ ->
                            ( model, Cmd.none )

                AddingChartable (Just (TrackableOption trackableId)) ->
                    case userData |> UserData.getTrackable trackableId of
                        Just trackable ->
                            let
                                newTrackableModel =
                                    Chart.Trackable.init trackableOptions True trackableId trackable "1" False True

                                userData_ =
                                    userData |> UserData.updateLineChart model.chartId (LC.addTrackable trackableId trackable)

                                trackableOptions =
                                    buildTrackableOptions userData_ model.chartId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | elements = m.elements |> Array.push (TrackableElement newTrackableModel)
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
                |> updateChart (Chart.update userData chartMsg)

        MoveDataClicked ->
            let
                userData_ =
                    userData |> UserData.moveData model.chart.graph.today
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
            Array.length model.elements
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
                (model.elements
                    |> Array.indexedMap
                        (\i d ->
                            case d of
                                ChartableElement c ->
                                    Chart.Chartable.view
                                        { canMoveUp = i > 0
                                        , canMoveDown = i < dataCount - 1
                                        , isSelected = model.chart.graph.selectedDataSet == Just i
                                        }
                                        c
                                        |> List.map (Html.map (ChartableMsg i))

                                TrackableElement t ->
                                    Chart.Trackable.view
                                        { canMoveUp = i > 0
                                        , canMoveDown = i < dataCount - 1
                                        , isSelected = model.chart.graph.selectedDataSet == Just i
                                        }
                                        t
                                        |> List.map (Html.map (TrackableMsg i))
                        )
                    |> Array.toList
                    |> List.concat
                )
                    ++ [ case model.addState of
                            AddingChartable addingChartableId ->
                                let
                                    toString option =
                                        case option of
                                            ChartableOption id ->
                                                "c" ++ CId.toString id

                                            TrackableOption id ->
                                                "t" ++ TId.toString id

                                    fromString str =
                                        case String.toList str of
                                            'c' :: cs ->
                                                Maybe.map ChartableOption <| CId.fromString <| String.fromList cs

                                            't' :: cs ->
                                                Maybe.map TrackableOption <| TId.fromString <| String.fromList cs

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
                                        , onClickStopPropagation ChartableAddCancelClicked
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
