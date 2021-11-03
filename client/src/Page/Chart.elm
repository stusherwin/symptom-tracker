module Page.Chart exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Dom as Dom
import Chart.Chartable
import Chart.LineChart as Chart
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
import UserData.LineChart as LineChart exposing (LineChart)
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable exposing (TrackableData(..))
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Model =
    { chartId : LineChartId
    , chart : Chart.Model
    , chartableOptions : List ( ChartableId, String )
    , addState : EditState
    , userData : UserData
    , chartables : List ( ChartableId, Chart.Chartable.Model )
    , nameIsPristine : Bool
    , trackableOptions : List ( TrackableId, ( String, Bool ) )
    }


type EditState
    = NotAdding
    | AddingChartable (Maybe ChartableId)


init : Date -> UserData -> LineChartId -> LineChart -> ( Model, Cmd Msg )
init today userData chartId chart =
    let
        ( chartModel, chartCmd ) =
            Chart.init today userData chartId chart

        trackableOptions =
            UserData.activeTrackables userData
                |> List.map (Tuple.mapSecond (Tuple.mapFirst .question))
                |> List.sortBy (String.toUpper << Tuple.first << Tuple.second)
    in
    ( { chartId = chartId
      , chart = chartModel
      , chartableOptions =
            UserData.chartables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .name))
                |> List.sortBy (String.toUpper << Tuple.second)
      , addState = NotAdding
      , userData = userData
      , chartables =
            chart.chartables
                |> List.filterMap
                    (\( chartableId, visible ) ->
                        userData
                            |> UserData.getChartable chartableId
                            |> Maybe.map (\c -> ( chartableId, Chart.Chartable.init userData trackableOptions True ( c, visible ) ))
                    )
      , nameIsPristine = True
      , trackableOptions = trackableOptions
      }
    , Cmd.batch
        [ Task.attempt (always NoOp) <| Dom.focus <| "chart-name"
        , Cmd.map ChartMsg chartCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChartMsg (Chart.subscriptions model.chart)



-- UPDATE


type Msg
    = NoOp
    | ChartMsg Chart.Msg
    | ChartableMsg Chart.Chartable.Msg
    | ChartNameUpdated String
    | ChartableAddClicked
    | ChartableToAddChanged (Maybe ChartableId)
    | ChartableAddConfirmClicked
    | ChartableAddCancelClicked
    | UserDataUpdated UserData
    | MoveDataClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChart fn m =
            { m | chart = m.chart |> fn }

        updateChartable chartableId fn m =
            { m | chartables = m.chartables |> Listx.updateLookup chartableId fn }

        updateChartableOptions fn m =
            { m | chartableOptions = m.chartableOptions |> fn |> List.sortBy (String.toUpper << Tuple.second) }

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

        ChartableMsg chartableMsg ->
            case chartableMsg of
                Chart.Chartable.ChartableHovered chartableId ->
                    ( model |> (updateChart <| Chart.hoverDataSet chartableId), Cmd.none )

                Chart.Chartable.ChartableEditClicked chartableId ->
                    ( model |> (updateChart <| Chart.selectDataSet (Just chartableId))
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
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.toggleChartableVisible chartableId)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.toggleDataSetVisible chartableId)
                        |> (updateChartable chartableId <| \c -> { c | visible = not c.visible })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableUpClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.moveChartableUp chartableId)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.moveDataSetBack chartableId)
                        |> (\m -> { m | chartables = m.chartables |> Listx.moveHeadwardsBy Tuple.first chartableId })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableDownClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.moveChartableDown chartableId)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.moveDataSetForward chartableId)
                        |> (\m -> { m | chartables = m.chartables |> Listx.moveTailwardsBy Tuple.first chartableId })
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
                        |> (updateChartableOptions <| Listx.insertLookup chartableId (Stringx.withDefault "[no name]" name))
                        |> (updateChart <| Chart.updateDataSetName chartableId name)
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
                                |> (updateChart <| Chart.updateDataSetColour userData_ chartableId)
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
                        |> (updateChart <| Chart.updateDataSetData userData_ chartableId)
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableDeleteClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.updateLineChart model.chartId (LineChart.deleteChartable chartableId)
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChart <| Chart.selectDataSet Nothing)
                        |> (updateChart <| Chart.removeDataSet chartableId)
                        |> (\m ->
                                { m
                                    | chartables = m.chartables |> List.filter (\( cId, _ ) -> cId /= chartableId)
                                    , addState = NotAdding
                                }
                           )
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.TrackableChanged chartableId trackableId (Just newTrackableId) ->
                    let
                        trackableOptionM =
                            model.trackableOptions |> Listx.lookup newTrackableId

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
                                            }
                                   )
                                |> (updateChart <| Chart.updateDataSetData userData_ chartableId)
                                |> (updateChart <| Chart.updateDataSetColour userData_ chartableId)
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
                        |> (updateChart <| Chart.updateDataSetData userData_ chartableId)
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
                                        model.trackableOptions
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
                                            }
                                   )
                                |> (updateChart <| Chart.updateDataSetData userData_ chartableId)
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
                                            }
                                   )
                                |> (updateChart <| Chart.updateDataSetData userData_ chartableId)
                            , Task.perform UserDataUpdated <| Task.succeed userData_
                            )

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
                                        |> List.map Tuple.first
                                        |> List.filter (\tId -> not (List.member tId <| List.map Tuple.first m.chart.graph.data))
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
            let
                ( chartableIdM, userData_, isNew ) =
                    case model.addState of
                        AddingChartable (Just chartableId) ->
                            ( Just chartableId, model.userData, False )

                        AddingChartable _ ->
                            let
                                ( chartableId, userData ) =
                                    model.userData
                                        |> UserData.addChartable
                                            { name = ""
                                            , colour = Nothing
                                            , inverted = False
                                            , sum = []
                                            }
                            in
                            ( chartableId, userData, True )

                        _ ->
                            ( Nothing, model.userData, False )

                chartableM =
                    chartableIdM |> Maybe.andThen (\chartableId -> userData_ |> UserData.getChartable chartableId)

                newChartableModelM =
                    chartableM |> Maybe.map (\c -> Chart.Chartable.init userData_ model.trackableOptions True ( c, True ))

                userDataM__ =
                    chartableIdM
                        |> Maybe.map
                            (\chartableId ->
                                userData_ |> UserData.updateLineChart model.chartId (LineChart.addChartable chartableId)
                            )
            in
            case ( chartableIdM, newChartableModelM, userDataM__ ) of
                ( Just chartableId, Just newChartableModel, Just userData__ ) ->
                    ( model
                        |> setUserData userData__
                        |> (updateChart <|
                                Chart.selectDataSet
                                    (if isNew then
                                        Just chartableId

                                     else
                                        Nothing
                                    )
                           )
                        |> (\m ->
                                { m
                                    | chartables = m.chartables |> Listx.insertLookup chartableId newChartableModel
                                    , addState = NotAdding
                                }
                           )
                        |> (updateChart <| Chart.addDataSet userData_ chartableId)
                        |> (updateChartableOptions <| Listx.insertLookup chartableId (Stringx.withDefault "[no name]" newChartableModel.name))
                    , Cmd.batch
                        [ Task.perform UserDataUpdated <| Task.succeed userData__
                        , if isNew then
                            Task.attempt (always NoOp) <| Dom.focus <| "chartable" ++ ChartableId.toString chartableId ++ "-name"

                          else
                            Cmd.none
                        ]
                    )

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
            [ text <| Stringx.withDefault "[no name]" model.chart.name ]
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
                (model.chartables
                    |> List.concatMap
                        (Chart.Chartable.view
                            ((Maybe.map Tuple.first << List.head) model.chart.graph.data)
                            ((Maybe.map Tuple.first << List.head << List.reverse) model.chart.graph.data)
                            model.chart.graph.selectedDataSet
                        )
                    |> List.map (Html.map ChartableMsg)
                )
                    ++ [ case model.addState of
                            AddingChartable addingChartableId ->
                                div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                                    [ Controls.textDropdown "w-full h-10"
                                        ChartableToAddChanged
                                        ChartableId.toString
                                        ChartableId.fromString
                                        (model.chartableOptions
                                            |> List.sortBy (String.toUpper << Tuple.second)
                                            |> List.map
                                                (\( cId, name ) ->
                                                    ( ( cId
                                                      , not <| List.member cId <| List.map Tuple.first model.chart.graph.data
                                                      )
                                                    , name
                                                    )
                                                )
                                        )
                                        (Just "New chartable")
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
                                    [ Controls.button "" Controls.ButtonGrey ChartableAddClicked SolidPlusCircle "Add chartable" True

                                    -- , Controls.button "ml-4" Controls.ButtonGrey MoveDataClicked SolidPlusCircle "Move data" True
                                    ]
                       ]
            ]
        ]
