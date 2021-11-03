module Page.Chartables exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Chart.Chartable
import Chart.LineChart as Chart exposing (DataSetId(..))
import Colour exposing (Colour(..))
import Controls
import Dict
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick)
import Htmlx
import IdDict
import Listx
import Platform.Cmd as Cmd
import Svg.Icon exposing (IconType(..), icon)
import Task
import UserData exposing (UserData)
import UserData.Chartable as Chartable exposing (Chartable)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChart as LineChart exposing (LineChartData(..))
import UserData.Trackable as Trackable exposing (Trackable, TrackableData(..))
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Model =
    { chartables : List ( ChartableId, Chart.Chartable.Model )
    , userData : UserData
    , editState : EditState
    }


type EditState
    = NotEditing
    | EditingChartable ChartableId


init : UserData -> Model
init userData =
    { chartables =
        UserData.activeChartables userData
            |> (List.map <|
                    \( id, ( c, v ) ) ->
                        let
                            canDelete =
                                UserData.lineCharts userData
                                    |> IdDict.values
                                    |> List.concatMap (Array.toList << .data)
                                    |> List.map Tuple.first
                                    |> List.filterMap
                                        (\dataSetId ->
                                            case dataSetId of
                                                LineChart.Chartable cId ->
                                                    Just cId

                                                _ ->
                                                    Nothing
                                        )
                                    |> List.member id
                                    |> not
                        in
                        ( id, Chart.Chartable.init userData canDelete id ( c, v ) )
               )
    , userData = userData
    , editState = NotEditing
    }



-- UPDATE


type Msg
    = NoOp
    | ChartableAddClicked
    | ChartableMsg Chart.Chartable.Msg
    | UserDataUpdated UserData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChartableAddClicked ->
            let
                chartable =
                    { name = ""
                    , colour = Nothing
                    , inverted = False
                    , sum = []
                    }

                ( idM, userData_ ) =
                    model.userData |> UserData.addChartable chartable
            in
            case idM of
                Just id ->
                    ( { model
                        | chartables = model.chartables |> Listx.insertLookup id (Chart.Chartable.init userData_ True id ( chartable, True ))
                        , editState = EditingChartable id
                      }
                    , Cmd.batch
                        [ Task.perform UserDataUpdated <| Task.succeed userData_
                        , Dom.getViewport
                            |> Task.andThen (\info -> Dom.setViewport 0 info.scene.height)
                            |> (Task.andThen <| always <| Dom.focus ("chartable" ++ ChartableId.toString id ++ "-name"))
                            |> Task.attempt (always NoOp)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        ChartableMsg chartableMsg ->
            let
                updateChartable chartableId fn m =
                    { m | chartables = m.chartables |> Listx.updateLookup chartableId fn }

                setUserData userData_ m =
                    { m | userData = userData_ }
            in
            case chartableMsg of
                Chart.Chartable.ChartableEditClicked chartableId ->
                    ( { model | editState = EditingChartable chartableId }
                    , Dom.focus ("chartable" ++ ChartableId.toString chartableId ++ "-name")
                        |> Task.attempt (always NoOp)
                    )

                Chart.Chartable.ChartableCloseClicked ->
                    ( { model | editState = NotEditing }
                    , Cmd.none
                    )

                Chart.Chartable.ChartableVisibleClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.toggleChartableVisible chartableId
                    in
                    ( model
                        |> setUserData userData_
                        |> (updateChartable chartableId <| \c -> { c | visible = not c.visible })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableUpClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.moveChartableUp chartableId
                    in
                    ( model
                        |> setUserData userData_
                        |> (\m -> { m | chartables = m.chartables |> Listx.moveHeadwardsBy Tuple.first chartableId })
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableDownClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.moveChartableDown chartableId
                    in
                    ( model
                        |> setUserData userData_
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
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.ChartableDeleteClicked chartableId ->
                    let
                        userData_ =
                            model.userData |> UserData.deleteChartable chartableId
                    in
                    ( model
                        |> setUserData userData_
                        |> (\m ->
                                { m
                                    | chartables = m.chartables |> List.filter (\( cId, _ ) -> cId /= chartableId)
                                    , editState = NotEditing
                                }
                           )
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                Chart.Chartable.TrackableChanged chartableId trackableId (Just newTrackableId) ->
                    let
                        trackableOptionM =
                            model.chartables
                                |> Listx.lookup chartableId
                                |> Maybe.map .trackableOptions
                                |> Maybe.withDefault []
                                |> Listx.lookup newTrackableId

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
                    , case userDataM_ of
                        Just _ ->
                            Task.perform UserDataUpdated <| Task.succeed userData_

                        _ ->
                            Cmd.none
                    )

                Chart.Chartable.TrackableAddClicked chartableId ->
                    let
                        -- chartableM =
                        --     UserData.getChartable chartableId model.userData
                        trackableM =
                            -- chartableM
                            --     |> Maybe.map (List.map Tuple.first << .sum)
                            --     |> Maybe.map
                            --         (\tIds ->
                            model.chartables
                                |> Listx.lookup chartableId
                                |> Maybe.map .trackableOptions
                                |> Maybe.withDefault []
                                |> List.filter (\( _, ( _, visible ) ) -> visible)
                                -- && not (List.member tId tIds))
                                |> List.map Tuple.first
                                -- )
                                |> List.head
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
                            , Task.perform UserDataUpdated <| Task.succeed userData_
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { chartables, editState } =
    let
        firstChartable =
            (Maybe.map (ChartableId << Tuple.first) << List.head) chartables

        lastChartable =
            (Maybe.map (ChartableId << Tuple.first) << List.head << List.reverse) chartables

        selectedChartable =
            case editState of
                EditingChartable id ->
                    Just (ChartableId id)

                _ ->
                    Nothing
    in
    div [ class "bg-white" ]
        [ h2 [ class "py-4 font-bold text-2xl text-center" ]
            [ text <| "Chartables" ]
        , div [ class "" ] <|
            (List.concatMap (Chart.Chartable.view firstChartable lastChartable selectedChartable) chartables
                |> List.map (Html.map ChartableMsg)
            )
                ++ [ div [ class "bg-gray-300 border-t-4 border-gray-400 flex" ]
                        [ Controls.button "mx-4 my-2" Controls.ButtonGrey ChartableAddClicked SolidPlusCircle "Add new chartable" True
                        ]
                   ]
        ]
