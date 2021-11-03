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
    | ChartableMsg Int Chart.Chartable.Msg
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

        ChartableMsg i chartableMsg ->
            case model.chartables |> Array.fromList |> Array.get i of
                Just ( chartableId, chartable ) ->
                    let
                        updateChartable : (Chart.Chartable.Model -> ( Chart.Chartable.Model, Cmd Chart.Chartable.Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                        updateChartable fn ( m, c ) =
                            let
                                ( chartable_, c_ ) =
                                    fn chartable
                            in
                            ( { m | chartables = m.chartables |> Array.fromList |> Array.set i ( chartableId, chartable_ ) |> Array.toList }
                            , Cmd.batch [ c, Cmd.map (ChartableMsg i) c_ ]
                            )

                        setUserData userData_ ( m, cmd ) =
                            ( { m | userData = userData_ }
                            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
                            )
                    in
                    case chartableMsg of
                        Chart.Chartable.ChartableEditClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update model.userData chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | editState = EditingChartable chartableId })

                        Chart.Chartable.ChartableCloseClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update model.userData chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | editState = NotEditing })

                        Chart.Chartable.ChartableVisibleClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.toggleChartableVisible chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableUpClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.moveChartableUp chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | chartables = m.chartables |> Listx.moveHeadwardsBy Tuple.first chartableId })

                        Chart.Chartable.ChartableDownClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.moveChartableDown chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | chartables = m.chartables |> Listx.moveTailwardsBy Tuple.first chartableId })

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

                        Chart.Chartable.ChartableColourUpdated colour ->
                            let
                                userData_ =
                                    model.userData
                                        |> UserData.updateChartable chartableId (Chartable.setColour colour)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableInvertedChanged inverted ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.setInverted inverted)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableDeleteClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.deleteChartable chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | chartables = m.chartables |> List.filter (\( cId, _ ) -> cId /= chartableId)
                                                , editState = NotEditing
                                            }
                                   )

                        Chart.Chartable.TrackableChanged trackableId (Just newTrackableId) ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.replaceTrackable trackableId newTrackableId)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

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

                        Chart.Chartable.TrackableAddClicked (Just newTrackableId) ->
                            let
                                trackableM =
                                    model.chartables
                                        |> Listx.lookup chartableId
                                        |> Maybe.map .trackableOptions
                                        |> Maybe.withDefault []
                                        |> List.filter (\( _, ( _, visible ) ) -> visible)
                                        |> List.map Tuple.first
                                        |> List.head
                                        |> Maybe.map (\tId -> ( tId, 1.0 ))

                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.addTrackable ( newTrackableId, 1 ))
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.TrackableDeleteClicked trackableId ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (Chartable.deleteTrackable trackableId)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { chartables, editState } =
    div [ class "bg-white" ]
        [ h2 [ class "py-4 font-bold text-2xl text-center" ]
            [ text <| "Chartables" ]
        , div [ class "" ] <|
            (chartables
                |> List.indexedMap
                    (\i ( cId, c ) ->
                        Chart.Chartable.view { canMoveUp = i > 0, canMoveDown = i < List.length chartables - 1, isSelected = editState == EditingChartable cId, isAnySelected = editState /= NotEditing } c
                            |> List.map (Html.map <| ChartableMsg i)
                    )
                |> List.concat
            )
                ++ [ div [ class "bg-gray-300 border-t-4 border-gray-400 flex" ]
                        [ Controls.button "mx-4 my-2" Controls.ButtonGrey ChartableAddClicked SolidPlusCircle "Add new chartable" True
                        ]
                   ]
        ]
