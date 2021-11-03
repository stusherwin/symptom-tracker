port module Page.Charts exposing (Model, Msg(..), init, subscriptions, update, view)

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
import IdDict
import Listx
import Maybe exposing (Maybe)
import Stringx
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), fillIcon, icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as Chartable exposing (Chartable, ChartableDict)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChart as LineChart exposing (LineChart)
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable as Trackable exposing (TrackableData(..), TrackableDict)
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Model =
    { today : Date
    , charts : List ( LineChartId, Graph.Model LineChartModel ChartableId ChartableModel )
    , userData : UserData
    , trackableOptions : List ( TrackableId, ( String, Bool ) )
    , chartableOptions : List ( ChartableId, String )
    , fullScreen : Bool
    }


type alias LineChartModel =
    { addState : EditState
    , viewport : Maybe Dom.Viewport
    , expandedValue : Bool
    }


type EditState
    = NotAdding
    | AddingChartable (Maybe ChartableId)


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
                |> IdDict.map (\_ chart -> toChartModel today userData chart)
                |> IdDict.toList
      , trackableOptions =
            UserData.activeTrackables userData
                |> List.map (Tuple.mapSecond (Tuple.mapFirst .question))
                |> List.sortBy (String.toUpper << Tuple.first << Tuple.second)
      , chartableOptions =
            UserData.chartables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .name))
                |> List.sortBy (String.toUpper << Tuple.second)
      , userData = userData
      , fullScreen = False
      }
    , Cmd.batch <|
        (UserData.lineCharts userData
            |> IdDict.keys
            |> List.map
                (\id ->
                    let
                        chartId =
                            "chart" ++ LineChartId.toString id ++ "-scrollable"
                    in
                    Dom.getViewportOf chartId
                        |> Task.andThen (\info -> Dom.setViewportOf chartId info.scene.width 0)
                        |> Task.attempt (always NoOp)
                )
        )
            ++ (UserData.lineCharts userData
                    |> IdDict.keys
                    |> List.map
                        (\chartId ->
                            Task.map2 Tuple.pair
                                (Dom.getViewportOf <| "chart" ++ LineChartId.toString chartId ++ "-scrollable")
                                (Dom.getElement <| "chart" ++ LineChartId.toString chartId ++ "-svg")
                                |> Task.attempt (ViewportUpdated chartId)
                        )
               )
    )


toChartModel : Date -> UserData -> LineChart -> Graph.Model LineChartModel ChartableId ChartableModel
toChartModel today userData chart =
    let
        toChartableModel_ ( chartableId, visible ) =
            userData
                |> UserData.getChartable chartableId
                |> Maybe.map (\c -> ( chartableId, toChartableModel userData visible c ))
    in
    { today = today
    , fillLines = chart.fillLines
    , data =
        chart.chartables
            |> List.filterMap toChartableModel_
    , selectedDataSet = Nothing
    , leavingDataSet = Nothing
    , hoveredDataSet = Nothing
    , selectedDataPoint = Nothing
    , hoveredDataPoint = Nothing
    , addState = NotAdding
    , viewport = Nothing
    , expandedValue = False
    , xScale = 1
    , minWidth = 0
    , currentWidth = 0
    }


toChartableModel : UserData -> Bool -> Chartable -> Graph.DataSet ChartableModel
toChartableModel userData visible chartable =
    { name = chartable.name
    , colour = toColour userData chartable
    , dataPoints = toDataPoints userData chartable
    , inverted = chartable.inverted
    , trackables = chartable.sum |> List.filterMap (toTrackableModel userData)
    , visible = visible
    , nameIsPristine = True
    }


toTrackableModel : UserData -> ( TrackableId, Float ) -> Maybe ( TrackableId, TrackableModel )
toTrackableModel userData ( trackableId, multiplier ) =
    userData
        |> UserData.getTrackable trackableId
        |> Maybe.map
            (\{ question } ->
                ( trackableId
                , { question = question
                  , multiplier = String.fromFloat multiplier
                  , isValid = True
                  }
                )
            )


toColour : UserData -> Chartable -> Colour
toColour userData chartable =
    let
        firstColour =
            List.head chartable.sum
                |> Maybe.andThen ((\tId -> UserData.getTrackable tId userData) << Tuple.first)
                |> Maybe.map .colour
    in
    Maybe.withDefault Colour.Gray <|
        if List.length chartable.sum == 1 then
            firstColour

        else
            case chartable.colour of
                Just c ->
                    Just c

                Nothing ->
                    firstColour


toDataPoints : UserData -> Chartable -> Dict Int Float
toDataPoints userData chartable =
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
                userData
                    |> UserData.getTrackable trackableId
                    |> Maybe.map
                        (Dict.map (\_ v -> v * multiplier) << Trackable.onlyFloatData)
            )
        |> List.foldl (Dictx.unionWith (\v1 v2 -> v1 + v2)) Dict.empty
        |> (if chartable.inverted then
                invert

            else
                identity
           )



-- PORTS


port toggleElementFullScreen : String -> Cmd msg


port fullScreenChanged : (Bool -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    fullScreenChanged FullScreenChanged



-- UPDATE


type Msg
    = NoOp
    | ChartFillLinesChecked LineChartId Bool
    | ChartShowPointsChecked LineChartId Bool
    | ChartFullScreenClicked LineChartId
    | ChartZoomOutClicked LineChartId
    | ChartZoomInClicked LineChartId
    | ChartClicked LineChartId
    | ChartableHovered LineChartId (Maybe ChartableId)
    | ChartableClicked LineChartId ChartableId
    | ChartableEditClicked LineChartId ChartableId
    | ChartableCloseClicked LineChartId
    | ChartableVisibleClicked LineChartId ChartableId
    | ChartableUpClicked LineChartId ChartableId
    | ChartableDownClicked LineChartId ChartableId
    | ChartableChanged LineChartId ChartableId (Maybe ChartableId)
    | ChartableNameUpdated LineChartId ChartableId String
    | ChartableColourUpdated LineChartId ChartableId (Maybe Colour)
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
    | UserDataUpdated UserData
    | ExpandValueClicked LineChartId
    | GraphMsg LineChartId (Graph.Msg ChartableId)
    | FullScreenChanged Bool
    | ViewportUpdated LineChartId (Result Dom.Error ( Dom.Viewport, Dom.Element ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChartModel chartId fn m =
            { m | charts = m.charts |> Listx.updateLookup chartId fn }

        updateChartableModel chartableId fn c =
            { c | data = c.data |> Listx.updateLookup chartableId fn }

        updateChartableOptions fn m =
            { m | chartableOptions = m.chartableOptions |> fn |> List.sortBy (String.toUpper << Tuple.second) }

        setUserData userData_ m =
            { m | userData = userData_ }
    in
    case msg of
        ChartFillLinesChecked chartId fl ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.setFillLines fl)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| \c -> { c | fillLines = fl })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartFullScreenClicked chartId ->
            ( model, toggleElementFullScreen ("chart" ++ LineChartId.toString chartId) )

        ChartClicked chartId ->
            ( model |> updateChartModel chartId (\c -> { c | selectedDataSet = Nothing, selectedDataPoint = Nothing }), Cmd.none )

        ChartZoomOutClicked chartId ->
            ( model |> updateChartModel chartId (\c -> { c | xScale = c.xScale * 3 / 4 })
            , Task.map2 Tuple.pair
                (Dom.getViewportOf <| "chart" ++ LineChartId.toString chartId ++ "-scrollable")
                (Dom.getElement <| "chart" ++ LineChartId.toString chartId ++ "-svg")
                |> Task.attempt (ViewportUpdated chartId)
            )

        ChartZoomInClicked chartId ->
            ( model |> updateChartModel chartId (\c -> { c | xScale = c.xScale * 4 / 3 })
            , Task.map2 Tuple.pair
                (Dom.getViewportOf <| "chart" ++ LineChartId.toString chartId ++ "-scrollable")
                (Dom.getElement <| "chart" ++ LineChartId.toString chartId ++ "-svg")
                |> Task.attempt (ViewportUpdated chartId)
            )

        ChartableHovered chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.hoverDataSet chartableId), Cmd.none )

        ChartableClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.toggleDataSetSelected chartableId), Cmd.none )

        ChartableEditClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.selectDataSet (Just chartableId) {- << (\c -> { c | editState = EditingChartable chartableId False }) -})
            , Task.attempt (always NoOp) <| Dom.focus <| "chart" ++ LineChartId.toString chartId ++ "-chartable" ++ ChartableId.toString chartableId ++ "-name"
            )

        ChartableCloseClicked chartId ->
            ( model |> (updateChartModel chartId <| Graph.selectDataSet Nothing << (\c -> { c | expandedValue = False })), Cmd.none )

        ChartableVisibleClicked chartId chartableId ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.toggleChartableVisible chartableId)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| Graph.toggleDataSetVisible chartableId)
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableUpClicked chartId chartableId ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.moveChartableUp chartableId)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| \c -> { c | data = c.data |> Listx.moveHeadwardsBy Tuple.first chartableId })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableDownClicked chartId chartableId ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.moveChartableDown chartableId)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| \c -> { c | data = c.data |> Listx.moveTailwardsBy Tuple.first chartableId })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableNameUpdated chartId chartableId name ->
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
                |> (updateChartModel chartId <|
                        updateChartableModel chartableId <|
                            \c -> { c | name = name, nameIsPristine = False }
                   )
                |> (updateChartableOptions <| Listx.insertLookup chartableId (Stringx.withDefault "[no name]" name))
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableColourUpdated chartId chartableId colour ->
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
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c -> { c | colour = toColour userData_ chartable_ }
                           )
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                _ ->
                    ( model, Cmd.none )

        ChartableInvertedChanged chartId chartableId inverted ->
            let
                userData_ =
                    model.userData |> UserData.updateChartable chartableId (Chartable.setInverted inverted)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <|
                        updateChartableModel chartableId <|
                            \c ->
                                { c
                                    | inverted = inverted
                                    , dataPoints =
                                        userData_
                                            |> UserData.getChartable chartableId
                                            |> Maybe.map (toDataPoints userData_)
                                            |> Maybe.withDefault c.dataPoints
                                }
                   )
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableAddClicked chartId ->
            ( model
                |> (updateChartModel chartId <|
                        Graph.selectDataSet Nothing
                            << (\c ->
                                    { c
                                        | addState =
                                            AddingChartable
                                                (model.chartableOptions
                                                    |> List.map Tuple.first
                                                    |> List.filter (\tId -> not (List.member tId <| List.map Tuple.first c.data))
                                                    |> List.head
                                                )
                                    }
                               )
                   )
            , Cmd.none
            )

        ChartableToAddChanged chartId chartableId ->
            ( model
                |> (updateChartModel chartId <|
                        Graph.selectDataSet Nothing
                            << (\c ->
                                    { c
                                        | addState = AddingChartable chartableId
                                    }
                               )
                   )
            , Cmd.none
            )

        ChartableAddConfirmClicked chartId ->
            let
                ( chartableIdM, userData_, isNew ) =
                    case model.charts |> Listx.lookup chartId |> Maybe.map .addState of
                        Just (AddingChartable (Just chartableId)) ->
                            ( Just chartableId, model.userData, False )

                        Just (AddingChartable _) ->
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
                    chartableM |> Maybe.map (toChartableModel userData_ True)

                userDataM__ =
                    chartableIdM
                        |> Maybe.map
                            (\chartableId ->
                                userData_ |> UserData.updateLineChart chartId (LineChart.addChartable chartableId)
                            )
            in
            case ( chartableIdM, newChartableModelM, userDataM__ ) of
                ( Just chartableId, Just newChartableModel, Just userData__ ) ->
                    ( model
                        |> setUserData userData__
                        |> (updateChartModel chartId <|
                                Graph.selectDataSet
                                    (if isNew then
                                        Just chartableId

                                     else
                                        Nothing
                                    )
                                    << (\c ->
                                            { c
                                                | data = c.data |> Listx.insertLookup chartableId newChartableModel
                                                , addState = NotAdding
                                            }
                                       )
                           )
                        |> (updateChartableOptions <| Listx.insertLookup chartableId (Stringx.withDefault "[no name]" newChartableModel.name))
                    , Cmd.batch
                        [ Task.perform UserDataUpdated <| Task.succeed userData__
                        , if isNew then
                            Task.attempt (always NoOp) <| Dom.focus <| "chart" ++ LineChartId.toString chartId ++ "-chartable" ++ ChartableId.toString chartableId ++ "-name"

                          else
                            Cmd.none
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        ChartableAddCancelClicked chartId ->
            ( model
                |> (updateChartModel chartId <|
                        Graph.selectDataSet Nothing
                            << (\c ->
                                    { c
                                        | addState = NotAdding
                                    }
                               )
                   )
            , Cmd.none
            )

        ChartableDeleteClicked chartId chartableId ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.deleteChartable chartableId)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <|
                        Graph.selectDataSet Nothing
                            << (\c ->
                                    { c
                                        | data = c.data |> List.filter (\( cId, _ ) -> cId /= chartableId)
                                        , addState = NotAdding
                                    }
                               )
                   )
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        TrackableChanged chartId chartableId trackableId (Just newTrackableId) ->
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
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> Listx.updateLookupWithKey trackableId (\( _, t ) -> ( newTrackableId, { t | question = question } ))
                                            , colour = toColour userData_ chartable_
                                            , dataPoints = toDataPoints userData_ chartable_
                                        }
                           )
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                _ ->
                    ( model, Cmd.none )

        TrackableMultiplierUpdated chartId chartableId trackableId stringValue ->
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

                chartableM_ =
                    userDataM_ |> Maybe.andThen (UserData.getChartable chartableId)
            in
            ( model
                |> setUserData (Maybe.withDefault model.userData userDataM_)
                |> (updateChartModel chartId <|
                        updateChartableModel chartableId <|
                            \c ->
                                { c
                                    | trackables =
                                        c.trackables
                                            |> Listx.updateLookup trackableId (\t -> { t | multiplier = stringValue, isValid = multiplierM /= Nothing })
                                    , dataPoints =
                                        case ( chartableM_, userDataM_ ) of
                                            ( Just chartable_, Just userData_ ) ->
                                                toDataPoints userData_ chartable_

                                            _ ->
                                                c.dataPoints
                                }
                   )
            , case userDataM_ of
                Just userData_ ->
                    Task.perform UserDataUpdated <| Task.succeed userData_

                _ ->
                    Cmd.none
            )

        TrackableAddClicked chartId chartableId ->
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
                    trackableM |> Maybe.andThen (toTrackableModel model.userData)

                userDataM_ =
                    trackableM |> (Maybe.map <| \trackable -> model.userData |> UserData.updateChartable chartableId (Chartable.addTrackable trackable))

                chartableM_ =
                    userDataM_ |> Maybe.andThen (UserData.getChartable chartableId)
            in
            case ( trackableModelM, chartableM_, userDataM_ ) of
                ( Just ( trackableId, trackableModel ), Just chartable_, Just userData_ ) ->
                    ( model
                        |> setUserData userData_
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> Listx.insertLookup trackableId trackableModel
                                            , colour = toColour userData_ chartable_
                                            , dataPoints = toDataPoints userData_ chartable_
                                        }
                           )
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                _ ->
                    ( model, Cmd.none )

        TrackableDeleteClicked chartId chartableId trackableId ->
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
                        |> (updateChartModel chartId <|
                                updateChartableModel chartableId <|
                                    \c ->
                                        { c
                                            | trackables = c.trackables |> (List.filter <| \( tId, _ ) -> tId /= trackableId)
                                            , colour = toColour userData_ chartable_
                                            , dataPoints = toDataPoints userData_ chartable_
                                        }
                           )
                    , Task.perform UserDataUpdated <| Task.succeed userData_
                    )

                _ ->
                    ( model, Cmd.none )

        ExpandValueClicked chartId ->
            ( model
                |> (updateChartModel chartId <| \c -> { c | expandedValue = not c.expandedValue })
            , Cmd.none
            )

        GraphMsg chartId (Graph.MouseDown ( x, y )) ->
            ( model
                |> (updateChartModel chartId <|
                        \c ->
                            case c.viewport of
                                Just { scene } ->
                                    let
                                        xPerc =
                                            x / scene.width

                                        yPerc =
                                            y / scene.height
                                    in
                                    c |> Graph.selectNearestDataPoint ( xPerc, yPerc )

                                -- { c | point = Just ( xPerc, yPerc ) }
                                _ ->
                                    c
                   )
            , Cmd.none
            )

        GraphMsg chartId (Graph.MouseMove ( x, y )) ->
            ( model
                |> (updateChartModel chartId <|
                        \c ->
                            case c.viewport of
                                Just { scene } ->
                                    let
                                        xPerc =
                                            x / scene.width

                                        yPerc =
                                            y / scene.height
                                    in
                                    c |> Graph.hoverNearestDataPoint ( xPerc, yPerc )

                                _ ->
                                    c
                   )
            , Cmd.none
            )

        GraphMsg chartId graphMsg ->
            ( model |> (updateChartModel chartId <| Graph.update graphMsg), Cmd.none )

        FullScreenChanged fullScreen ->
            ( { model | fullScreen = fullScreen }, Cmd.none )

        ViewportUpdated chartId (Ok ( scrollable, svg )) ->
            ( model |> (updateChartModel chartId <| \c -> { c | viewport = Just scrollable, currentWidth = svg.element.width, minWidth = scrollable.viewport.width }), Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "bg-white"
        ]
    <|
        h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
            :: (model.charts
                    |> List.map (viewLineChart model.fullScreen model.chartableOptions model.trackableOptions model.userData)
               )


viewLineChart : Bool -> List ( ChartableId, String ) -> List ( TrackableId, ( String, Bool ) ) -> UserData -> ( LineChartId, Graph.Model LineChartModel ChartableId ChartableModel ) -> Html Msg
viewLineChart fullScreen chartableOptions trackableOptions userData ( chartId, model ) =
    let
        viewChartable ( chartableId, dataSet ) =
            let
                canMoveUp =
                    (Maybe.map Tuple.first << List.head) model.data /= Just chartableId

                canMoveDown =
                    (Maybe.map Tuple.first << List.head << List.reverse) model.data /= Just chartableId

                canEditColour =
                    List.length dataSet.trackables > 1

                options =
                    trackableOptions
                        |> List.map
                            (\( tId, ( question, visible ) ) ->
                                ( ( tId
                                  , visible
                                        && (not <|
                                                List.member tId <|
                                                    (dataSet.trackables
                                                        |> List.map Tuple.first
                                                    )
                                           )
                                  )
                                , question
                                )
                            )

                colour =
                    if not dataSet.visible || not ((model.selectedDataSet == Nothing && model.hoveredDataSet == Nothing) || model.selectedDataSet == Just chartableId || model.hoveredDataSet == Just chartableId) then
                        Colour.Gray

                    else
                        dataSet.colour
            in
            [ div
                [ class "border-t-4"
                , Colour.class "bg" colour
                , Colour.classUp "border" colour
                , onMouseEnter <| ChartableHovered chartId (Just chartableId)
                , onMouseLeave <| ChartableHovered chartId Nothing
                ]
                [ if model.selectedDataSet /= Just chartableId then
                    div
                        [ class "p-4 flex items-center"
                        ]
                        [ button
                            [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                            , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartId chartableId
                            ]
                            [ icon "w-5 h-5" <|
                                if dataSet.visible then
                                    SolidEye

                                else
                                    SolidEyeSlash
                            ]
                        , if dataSet.visible then
                            span [ class "ml-4 w-full", Htmlx.onClickStopPropagation NoOp ]
                                [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black", href "#", target "_self", Htmlx.onClickPreventDefault (ChartableClicked chartId chartableId) ]
                                    [ if model.selectedDataSet == Just chartableId then
                                        icon "w-5 h-5 relative -ml-1 mr-0.5" SolidCaretRight

                                      else
                                        span [] []
                                    , span []
                                        [ text <|
                                            if String.isEmpty dataSet.name then
                                                "[no name]"

                                            else
                                                dataSet.name
                                        ]
                                    , icon "absolute right-0 w-5 h-5" SolidPencilAlt
                                    ]
                                ]

                          else
                            span [ class "ml-4 w-full font-bold" ]
                                [ text <|
                                    if String.isEmpty dataSet.name then
                                        "[no name]"

                                    else
                                        dataSet.name
                                ]
                        , button
                            [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                            , Htmlx.onClickStopPropagation (ChartableDeleteClicked chartId chartableId)
                            ]
                            [ icon "w-5 h-5" <| SolidTrashAlt ]
                        , button
                            [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
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
                        ]

                  else
                    div
                        [ class "px-4 flex items-center"
                        , classList
                            [ ( "py-1", canEditColour )
                            , ( "py-2", not canEditColour )
                            ]
                        ]
                        [ button
                            [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                            , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartId chartableId
                            ]
                            [ icon "w-5 h-5" <|
                                if dataSet.visible then
                                    SolidEye

                                else
                                    SolidEyeSlash
                            ]
                        , Controls.textbox [ class "ml-4 w-72" ]
                            [ id <| "chart" ++ LineChartId.toString chartId ++ "-chartable" ++ ChartableId.toString chartableId ++ "-name"
                            , placeholder "Name"
                            ]
                            dataSet.name
                            { isValid = True, isRequired = True, isPristine = dataSet.nameIsPristine }
                            (ChartableNameUpdated chartId chartableId)
                        , label [ class "ml-12 flex-shrink-0 flex-grow-0 font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                        , input
                            [ type_ "checkbox"
                            , id "inverted"
                            , class "ml-2 flex-shrink-0 flex-grow-0"
                            , onCheck (ChartableInvertedChanged chartId chartableId)
                            , checked dataSet.inverted
                            ]
                            []
                        , if canEditColour then
                            Controls.colourDropdown "ml-4 flex-shrink-0 flex-grow-0" (ChartableColourUpdated chartId chartableId) (Just dataSet.colour) { showFilled = False }

                          else
                            span [ class "ml-4" ] []
                        , button
                            [ class "ml-auto rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                            , Htmlx.onClickStopPropagation (ChartableCloseClicked chartId)
                            ]
                            [ icon "w-5 h-5" <| SolidTimes ]
                        ]
                ]
            , if model.selectedDataSet == Just chartableId then
                div
                    [ class "p-4"
                    , Colour.classDown "bg" colour
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
                                        TrackableId.toString
                                        TrackableId.fromString
                                        (options |> List.map (\( ( tId, visible ), q ) -> ( ( tId, visible || tId == trackableId ), q )))
                                        Nothing
                                        (Just trackableId)
                                        { showFilled = False }
                                    , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                                    , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True, isPristine = False } (TrackableMultiplierUpdated chartId chartableId trackableId)
                                    , button
                                        [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , Htmlx.onClickStopPropagation (TrackableDeleteClicked chartId chartableId trackableId)
                                        ]
                                        [ icon "w-5 h-5" <| SolidTrashAlt ]
                                    ]
                            )
                    )
                        ++ [ div [ class "mt-4 first:mt-0 flex" ]
                                [ Controls.button "ml-9 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey (TrackableAddClicked chartId chartableId) SolidPlusCircle "Add trackable" (options |> List.any (Tuple.second << Tuple.first)) ]
                           ]

              else
                div [] []
            ]
    in
    div
        []
        [ div
            [ id ("chart" ++ LineChartId.toString chartId)
            , class "bg-white"
            , classList
                [ ( "p-8", fullScreen )
                ]
            ]
            [ div
                [ class "mx-4 my-0 flex scrollable-parent relative"
                , style "height" "300px"
                ]
                ([ viewJustYAxis "flex-grow-0 flex-shrink-0" model
                 , viewScrollableContainer ("chart" ++ LineChartId.toString chartId ++ "-scrollable") [ Html.map (GraphMsg chartId) <| viewLineGraph ("chart" ++ LineChartId.toString chartId ++ "-svg") "h-full" model ]
                 , div [ class "absolute right-2 top-6 flex flex-col" ]
                    [ button
                        [ class "rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none text-opacity-70 hover:bg-opacity-100 hover:text-opacity-100 focus:text-opacity-100"
                        , Htmlx.onClickStopPropagation (ChartFullScreenClicked chartId)
                        ]
                        [ icon "w-5 h-5" <|
                            if fullScreen then
                                SolidCompress

                            else
                                SolidExpand
                        ]
                    , button
                        [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none text-opacity-70 hover:bg-opacity-100 hover:text-opacity-100 focus:text-opacity-100"
                        , Htmlx.onClickStopPropagation (ChartZoomInClicked chartId)
                        ]
                        [ icon "w-5 h-5" SolidPlus
                        ]
                    , button
                        [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none"
                        , classList
                            [ ( "text-opacity-50 cursor-default", model.currentWidth <= model.minWidth )
                            , ( "text-opacity-70 hover:text-opacity-100 hover:bg-opacity-100 focus:text-opacity-100 focus:bg-opacity-100", model.currentWidth > model.minWidth )
                            ]
                        , Htmlx.onClickStopPropagation (ChartZoomOutClicked chartId)
                        , disabled (model.currentWidth <= model.minWidth)
                        ]
                        [ icon "w-5 h-5" <| SolidMinus
                        ]
                    , button
                        [ class "mt-2 rounded shadow p-2 bg-white bg-opacity-80 text-black focus:outline-none fill-icon text-opacity-70 hover:bg-opacity-100 focus:bg-opacity:100 focus:text-opacity-100"
                        , Htmlx.onClickStopPropagation (ChartFillLinesChecked chartId <| not model.fillLines)
                        ]
                        [ fillIcon "w-5 h-5" model.fillLines ]
                    ]
                 ]
                    ++ (case model.selectedDataSet |> Maybe.andThen (\id -> Listx.findBy Tuple.first id chartableOptions) of
                            Just ( id, name ) ->
                                [ div
                                    [ class "absolute left-10 top-6 rounded bg-white bg-opacity-80 p-2 min-w-44 max-w-xs" ]
                                    ([ button
                                        [ class "absolute right-2 top-2 text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , Htmlx.onClickStopPropagation (ChartableCloseClicked chartId)
                                        ]
                                        [ icon "w-4 h-4" SolidTimes
                                        ]
                                     , h4 [ class "font-bold pr-5" ] [ text name ]
                                     ]
                                        ++ (let
                                                featuredDataPoint =
                                                    case model.selectedDataPoint of
                                                        Just p ->
                                                            Just p

                                                        _ ->
                                                            model.hoveredDataPoint
                                            in
                                            case featuredDataPoint of
                                                Just date ->
                                                    let
                                                        data =
                                                            userData
                                                                |> UserData.getChartable id
                                                                |> Maybe.map
                                                                    (.sum
                                                                        >> List.filterMap
                                                                            (\( tId, m ) ->
                                                                                userData |> UserData.getTrackable tId |> Maybe.map (\t -> ( t.question, Maybe.withDefault 0 <| Dict.get date <| Trackable.onlyFloatData t, m ))
                                                                            )
                                                                    )
                                                                |> Maybe.withDefault []

                                                        total =
                                                            List.sum <| List.map (\( _, v, m ) -> v * m) <| data

                                                        invertedTotal =
                                                            let
                                                                inverted =
                                                                    userData
                                                                        |> UserData.getChartable id
                                                                        |> Maybe.map .inverted
                                                                        |> Maybe.withDefault False
                                                            in
                                                            if inverted then
                                                                let
                                                                    max =
                                                                        model.data
                                                                            |> Listx.lookup id
                                                                            |> Maybe.andThen (List.maximum << Dict.values << .dataPoints)
                                                                            |> Maybe.withDefault 0
                                                                in
                                                                Just ( max, max - total )

                                                            else
                                                                Nothing
                                                    in
                                                    [ p [ class "mt-2 text-sm" ] [ text <| Date.format "EEE d MMM y" <| Date.fromRataDie date ]
                                                    , p [ class "text-sm flex justify-between items-baseline" ]
                                                        [ span [] <|
                                                            [ text "Value"
                                                            , span [ class "ml-1 font-bold" ] [ text <| String.fromFloat total ]
                                                            ]
                                                                ++ (case invertedTotal of
                                                                        Just ( _, t ) ->
                                                                            [ span [ class "ml-1" ] [ text "(inverted " ]
                                                                            , span [ class "font-bold" ] [ text <| String.fromFloat t ]
                                                                            , span [ class "" ] [ text ")" ]
                                                                            ]

                                                                        _ ->
                                                                            []
                                                                   )
                                                        , a
                                                            [ href "#"
                                                            , target "_self"
                                                            , class "ml-2 text-sm text-blue-600 hover:text-blue-800 underline"
                                                            , Htmlx.onClickPreventDefault (ExpandValueClicked chartId)
                                                            ]
                                                          <|
                                                            if model.expandedValue then
                                                                [ text "less", icon "ml-1 w-3 h-3 inline" SolidAngleUp ]

                                                            else
                                                                [ text "more", icon "ml-1 w-3 h-3 inline" SolidAngleDown ]
                                                        ]
                                                    , if model.expandedValue then
                                                        div [ class "mt-2 pr-2 max-h-24 overflow-y-auto text-sm" ]
                                                            [ table [ class "w-full" ] <|
                                                                (data
                                                                    |> List.indexedMap
                                                                        (\i ( question, value, multiplier ) ->
                                                                            tr []
                                                                                [ td [ class "align-baseline" ]
                                                                                    [ icon "w-2 h-2" <|
                                                                                        if i == 0 then
                                                                                            SolidEquals

                                                                                        else
                                                                                            SolidPlus
                                                                                    ]
                                                                                , td [ class "pl-2 align-baseline" ] [ text question ]
                                                                                , td [ class "pl-2 align-baseline text-right" ] [ text <| String.fromFloat value ]
                                                                                , td [ class "pl-1 align-baseline" ] [ icon "w-2 h-2" SolidTimes ]
                                                                                , td [ class "pl-1 align-baseline text-right" ] [ text <| String.fromFloat multiplier ]
                                                                                , td [ class "pl-1 align-baseline " ] [ icon "w-2 h-2" SolidEquals ]
                                                                                , td [ class "pl-1 align-baseline text-right" ] [ text <| String.fromFloat (value * multiplier) ]
                                                                                ]
                                                                        )
                                                                )
                                                                    ++ (case invertedTotal of
                                                                            Just ( max, t ) ->
                                                                                [ tr []
                                                                                    [ td [ class "align-baseline" ] []
                                                                                    , td [ class "pl-2 align-baseline text-right", colspan 5 ] [ text <| "Total:" ]
                                                                                    , td [ class "pl-1 align-baseline text-right font-bold" ] [ text <| String.fromFloat total ]
                                                                                    ]
                                                                                , tr []
                                                                                    [ td [ class "align-baseline" ] []
                                                                                    , td [ class "pl-2 align-baseline text-right", colspan 2 ]
                                                                                        [ span [ class "" ] [ text "Inverted: " ]
                                                                                        , text <| String.fromFloat max ++ " (max value)"
                                                                                        ]
                                                                                    , td [ class "pl-1 align-baseline" ] [ icon "w-2 h-2" SolidMinus ]
                                                                                    , td [ class "pl-1 align-baseline text-right font-bold" ] [ text <| String.fromFloat total ]
                                                                                    , td [ class "pl-1 align-baseline " ] [ icon "w-2 h-2" SolidEquals ]
                                                                                    , td [ class "pl-1 align-baseline text-right font-bold" ] [ text <| String.fromFloat t ]
                                                                                    ]
                                                                                ]

                                                                            _ ->
                                                                                []
                                                                       )
                                                            ]

                                                      else
                                                        div [] []
                                                    ]

                                                _ ->
                                                    [ p [ class "mt-2 text-sm" ] [ text "Hover over or click on ", br [] [], text "a point to see its value" ] ]
                                           )
                                    )
                                ]

                            _ ->
                                []
                       )
                )
            ]
        , div [ class "mt-8 bg-gray-200" ] <|
            (model.data |> List.concatMap viewChartable)
                ++ [ case model.addState of
                        AddingChartable addingChartableId ->
                            div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                                [ Controls.textDropdown "w-full h-10"
                                    (ChartableToAddChanged chartId)
                                    ChartableId.toString
                                    ChartableId.fromString
                                    (chartableOptions
                                        |> List.sortBy (String.toUpper << Tuple.second)
                                        |> List.map
                                            (\( cId, name ) ->
                                                ( ( cId
                                                  , not <| List.member cId <| List.map Tuple.first model.data
                                                  )
                                                , name
                                                )
                                            )
                                    )
                                    (Just "New chartable")
                                    addingChartableId
                                    { showFilled = False }
                                , Controls.button "ml-4" Controls.ButtonGrey (ChartableAddConfirmClicked chartId) SolidPlusCircle "Add" True
                                , button
                                    [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                    , Htmlx.onClickStopPropagation (ChartableAddCancelClicked chartId)
                                    ]
                                    [ icon "w-5 h-5" <| SolidTimes ]
                                ]

                        _ ->
                            div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                                [ Controls.button "" Controls.ButtonGrey (ChartableAddClicked chartId) SolidPlusCircle "Add chartable" True
                                ]
                   ]
        ]


viewScrollableContainer : String -> List (Html msg) -> Html msg
viewScrollableContainer containerId children =
    div [ class "relative flex-grow" ]
        [ node "scrollable-container"
            [ id containerId
            , class "absolute overflow-x-scroll top-0 left-0 right-0 bottom-0"
            ]
            children
        ]
