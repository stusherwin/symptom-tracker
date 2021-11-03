module Page.Chartables exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Chart.Chartable
import Colour exposing (Colour(..))
import Controls
import Extra.Array as Array
import Extra.List as List
import Html exposing (..)
import Html.Attributes exposing (..)
import IdDict
import Platform.Cmd as Cmd
import Svg.Icon exposing (IconType(..))
import Task
import UserData exposing (UserData)
import UserData.Chartable as C
import UserData.ChartableId as CId exposing (ChartableId)
import UserData.LineChart as LC exposing (LineChart(..))
import UserData.Trackable as T exposing (Responses(..))


type alias Model =
    { chartables : Array ( ChartableId, Chart.Chartable.Model )
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
                                                LC.ChartableElement { chartableId } ->
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
    , editState = NotEditing
    }



-- UPDATE


type Msg
    = NoOp
    | ChartableAddClicked
    | ChartableMsg Int Chart.Chartable.Msg
    | UserDataUpdated UserData


update : UserData -> Msg -> Model -> ( Model, Cmd Msg )
update userData msg model =
    let
        setUserData userData_ ( m, cmd ) =
            ( m
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )
    in
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
                    userData |> UserData.addChartable newChartable
            in
            case idM of
                Just ( id, chartable ) ->
                    ( model, Cmd.none )
                        |> setUserData userData_
                        |> (Tuple.mapFirst <|
                                \m ->
                                    { m
                                        | chartables = model.chartables |> Array.push ( id, Chart.Chartable.init userData_ True id ( chartable, True ) )
                                        , editState = EditingChartable id
                                    }
                           )
                        |> (Tuple.mapSecond <|
                                \c ->
                                    Cmd.batch
                                        [ c
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
                    in
                    case chartableMsg of
                        Chart.Chartable.ChartableEditClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update userData chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | editState = EditingChartable chartableId })

                        Chart.Chartable.ChartableCloseClicked ->
                            ( model, Cmd.none )
                                |> (updateChartable <| Chart.Chartable.update userData chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | editState = NotEditing })

                        Chart.Chartable.ChartableVisibleClicked ->
                            let
                                userData_ =
                                    userData |> UserData.toggleChartableVisible chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableUpClicked ->
                            let
                                userData_ =
                                    userData |> UserData.moveChartableUp chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | chartables = m.chartables |> Array.swap i (i - 1) })

                        Chart.Chartable.ChartableDownClicked ->
                            let
                                userData_ =
                                    userData |> UserData.moveChartableDown chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (Tuple.mapFirst <| \m -> { m | chartables = m.chartables |> Array.swap i (i + 1) })

                        Chart.Chartable.ChartableNameUpdated name ->
                            let
                                userData_ =
                                    if not <| String.isEmpty name then
                                        userData |> UserData.updateChartable chartableId (C.setName name)

                                    else
                                        userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableColourUpdated colour ->
                            let
                                userData_ =
                                    userData
                                        |> UserData.updateChartable chartableId (C.setOwnColour colour)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableInvertedChanged inverted ->
                            let
                                userData_ =
                                    userData |> UserData.updateChartable chartableId (C.setInverted inverted)
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.ChartableDeleteClicked ->
                            let
                                userData_ =
                                    userData |> UserData.deleteChartable chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)
                                |> (Tuple.mapFirst <|
                                        \m ->
                                            { m
                                                | chartables = m.chartables |> Array.delete i
                                                , editState = NotEditing
                                            }
                                   )

                        Chart.Chartable.TrackableChanged trackableId (Just newTrackableId) ->
                            let
                                newTrackableM =
                                    userData |> UserData.getTrackable newTrackableId

                                userData_ =
                                    case newTrackableM of
                                        Just newTrackable ->
                                            userData |> UserData.updateChartable chartableId (C.replaceTrackable trackableId newTrackableId newTrackable)

                                        _ ->
                                            userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.TrackableMultiplierUpdated trackableId stringValue ->
                            let
                                userData_ =
                                    case T.parseMultiplier stringValue of
                                        Just multiplier ->
                                            userData |> UserData.updateChartable chartableId (C.setMultiplier trackableId multiplier)

                                        _ ->
                                            userData
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.TrackableAddClicked (Just newTrackableId) ->
                            let
                                newTrackableM =
                                    userData |> UserData.getTrackable newTrackableId

                                userData_ =
                                    case newTrackableM of
                                        Just newTrackable ->
                                            userData |> UserData.updateChartable chartableId (C.addTrackable newTrackableId newTrackable 1)

                                        _ ->
                                            userData

                                _ =
                                    userData_ |> UserData.getChartable chartableId
                            in
                            ( model, Cmd.none )
                                |> setUserData userData_
                                |> (updateChartable <| Chart.Chartable.update userData_ chartableMsg)

                        Chart.Chartable.TrackableDeleteClicked trackableId ->
                            let
                                userData_ =
                                    userData |> UserData.updateChartable chartableId (C.deleteTrackable trackableId)
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
