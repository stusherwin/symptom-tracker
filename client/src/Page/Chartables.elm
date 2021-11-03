module Page.Chartables exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Arrayx
import Browser.Dom as Dom
import Chart.Chartable
import Chart.LineChart as Chart exposing (DataSetId(..))
import Colour exposing (Colour(..))
import Controls
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick)
import IdDict
import Listx
import Platform.Cmd as Cmd
import Svg.Icon exposing (IconType(..), icon)
import Task
import UserData exposing (UserData)
import UserData.Chartable as C exposing (Chartable)
import UserData.ChartableId as CId exposing (ChartableId)
import UserData.LineChart as LC exposing (LineChart(..))
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId as TId exposing (TrackableId)


type alias Model =
    { chartables : Array ( ChartableId, Chart.Chartable.Model )
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
                                    |> List.concatMap (Array.toList << LC.dataSets)
                                    |> List.map Tuple.first
                                    |> List.filterMap
                                        (\dataSetId ->
                                            case dataSetId of
                                                LC.Chartable { chartableId } ->
                                                    Just chartableId

                                                _ ->
                                                    Nothing
                                        )
                                    |> List.member id
                                    |> not
                        in
                        ( id, Chart.Chartable.init userData canDelete id ( c, v ) )
               )
            |> Array.fromList
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
                newChartable =
                    { name = ""
                    , ownColour = Nothing
                    , isInverted = False
                    , sum = []
                    }

                ( idM, userData_ ) =
                    model.userData |> UserData.addChartable newChartable
            in
            case idM of
                Just ( id, chartable ) ->
                    ( { model
                        | chartables = model.chartables |> Array.push ( id, Chart.Chartable.init userData_ True id ( chartable, True ) )
                        , editState = EditingChartable id
                      }
                    , Cmd.batch
                        [ Task.perform UserDataUpdated <| Task.succeed userData_
                        , Dom.getViewport
                            |> Task.andThen (\info -> Dom.setViewport 0 info.scene.height)
                            |> (Task.andThen <| always <| Dom.focus ("chartable" ++ CId.toString id ++ "-name"))
                            |> Task.attempt (always NoOp)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        ChartableMsg i chartableMsg ->
            case model.chartables |> Array.get i of
                Just ( chartableId, chartable ) ->
                    let
                        updateChartable : (Chart.Chartable.Model -> ( Chart.Chartable.Model, Cmd Chart.Chartable.Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
                        updateChartable fn ( m, c ) =
                            let
                                ( chartable_, c_ ) =
                                    fn chartable
                            in
                            ( { m | chartables = m.chartables |> Array.set i ( chartableId, chartable_ ) }
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
                                |> (Tuple.mapFirst <| \m -> { m | chartables = m.chartables |> Arrayx.swap i (i - 1) })

                        Chart.Chartable.ChartableDownClicked ->
                            let
                                userData_ =
                                    model.userData |> UserData.moveChartableDown chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | chartables = m.chartables |> Arrayx.swap i (i + 1) })

                        Chart.Chartable.ChartableNameUpdated name ->
                            let
                                userData_ =
                                    if not <| String.isEmpty name then
                                        model.userData |> UserData.updateChartable chartableId (C.setName name)

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
                                        |> UserData.updateChartable chartableId (C.setOwnColour colour)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableInvertedChanged inverted ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (C.setInverted inverted)
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
                                                | chartables = m.chartables |> Arrayx.delete i
                                                , editState = NotEditing
                                            }
                                   )

                        Chart.Chartable.TrackableChanged trackableId (Just newTrackableId) ->
                            let
                                newTrackableM =
                                    model.userData |> UserData.getTrackable newTrackableId

                                userData_ =
                                    case newTrackableM of
                                        Just newTrackable ->
                                            model.userData |> UserData.updateChartable chartableId (C.replaceTrackable trackableId newTrackableId newTrackable)

                                        _ ->
                                            model.userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.TrackableMultiplierUpdated trackableId stringValue ->
                            let
                                userData_ =
                                    case T.parseMultiplier stringValue of
                                        Just multiplier ->
                                            model.userData |> UserData.updateChartable chartableId (C.setMultiplier trackableId multiplier)

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
                                        |> Array.get i
                                        |> Maybe.map (Tuple.second >> .trackableOptions)
                                        |> Maybe.withDefault []
                                        |> List.filter (\( _, ( _, visible ) ) -> visible)
                                        |> List.map Tuple.first
                                        |> List.head
                                        |> Maybe.map (\tId -> ( tId, 1.0 ))

                                newTrackableM =
                                    model.userData |> UserData.getTrackable newTrackableId

                                userData_ =
                                    case newTrackableM of
                                        Just newTrackable ->
                                            model.userData |> UserData.updateChartable chartableId (C.addTrackable newTrackableId newTrackable 1)

                                        _ ->
                                            model.userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.TrackableDeleteClicked trackableId ->
                            let
                                userData_ =
                                    model.userData |> UserData.updateChartable chartableId (C.deleteTrackable trackableId)
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
                |> Array.indexedMap
                    (\i ( cId, c ) ->
                        Chart.Chartable.view
                            { canMoveUp = i > 0
                            , canMoveDown = i < Array.length chartables - 1
                            , isSelected = editState == EditingChartable cId
                            }
                            c
                            |> List.map (Html.map <| ChartableMsg i)
                    )
                |> Array.toList
                |> List.concat
            )
                ++ [ div [ class "bg-gray-300 border-t-4 border-gray-400 flex" ]
                        [ Controls.button "mx-4 my-2" Controls.ButtonGrey ChartableAddClicked SolidPlusCircle "Add new chartable" True
                        ]
                   ]
        ]
