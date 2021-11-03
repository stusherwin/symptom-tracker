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
import IdDict
import Listx
import Maybe exposing (Maybe)
import Stringx
import Svg.Graph as Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Svg.Icon exposing (IconType(..), icon)
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
    , trackableOptions : List ( TrackableId, String )
    , chartableOptions : List ( ChartableId, String )
    }


type alias LineChartModel =
    { editState : EditState
    }


type EditState
    = NotEditing
    | AddingChartable (Maybe ChartableId)
    | EditingChartable ChartableId Bool


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
                |> List.filter (\( _, ( _, visible ) ) -> visible)
                |> List.map (\( id, ( t, _ ) ) -> ( id, t.question ))
                |> List.sortBy (String.toUpper << Tuple.second)
      , chartableOptions =
            UserData.chartables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .name))
                |> List.sortBy (String.toUpper << Tuple.second)
      , userData = userData
      }
    , Cmd.batch <|
        (UserData.lineCharts userData
            |> IdDict.keys
            |> List.map
                (\id ->
                    let
                        chartId =
                            "chart" ++ LineChartId.toString id
                    in
                    Dom.getViewportOf chartId
                        |> Task.andThen (\info -> Dom.setViewportOf chartId info.scene.width 0)
                        |> Task.attempt (always NoOp)
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
    , showPoints = chart.showPoints
    , data =
        chart.chartables
            |> List.filterMap toChartableModel_
    , selectedDataSet = Nothing
    , hoveredDataSet = Nothing
    , selectedDataPoint = Nothing
    , editState = NotEditing
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
    Maybe.withDefault Colour.Gray <|
        if List.length chartable.sum == 1 then
            List.head chartable.sum
                |> Maybe.andThen ((\tId -> UserData.getTrackable tId userData) << Tuple.first)
                |> Maybe.map .colour

        else
            chartable.colour


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



-- UPDATE


type Msg
    = NoOp
    | FillLinesChecked LineChartId Bool
    | ShowPointsChecked LineChartId Bool
    | ChartableHovered LineChartId (Maybe ChartableId)
    | ChartableEditClicked LineChartId ChartableId
    | ChartableCloseClicked LineChartId
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
    | UserDataUpdated UserData
    | GraphMsg LineChartId (Graph.Msg ChartableId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChartModel chartId fn m =
            { m | charts = m.charts |> Listx.updateLookup chartId fn }

        updateChartableModel chartableId fn c =
            { c | data = c.data |> Listx.updateLookup chartableId fn }

        updateChartableOptions fn m =
            { m | chartableOptions = m.chartableOptions |> fn |> List.sortBy (String.toUpper << Tuple.second) }

        updateSelectedDataSet c =
            { c
                | selectedDataSet =
                    case c.editState of
                        EditingChartable chartableId _ ->
                            Just chartableId

                        _ ->
                            Nothing
            }

        setUserData userData_ m =
            { m | userData = userData_ }
    in
    case msg of
        FillLinesChecked chartId fl ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.setFillLines fl)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| \c -> { c | fillLines = fl })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ShowPointsChecked chartId sp ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.setShowPoints sp)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| \c -> { c | showPoints = sp })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableHovered chartId chartableId ->
            ( model |> (updateChartModel chartId <| Graph.hoverDataSet chartableId), Cmd.none )

        ChartableEditClicked chartId chartableId ->
            ( model |> (updateChartModel chartId <| updateSelectedDataSet << (\c -> { c | editState = EditingChartable chartableId False }))
            , Task.attempt (always NoOp) <| Dom.focus <| "chart" ++ LineChartId.toString chartId ++ "-chartable" ++ ChartableId.toString chartableId ++ "-name"
            )

        ChartableCloseClicked chartId ->
            ( model |> (updateChartModel chartId <| updateSelectedDataSet << (\c -> { c | editState = NotEditing })), Cmd.none )

        ChartableVisibleClicked chartId chartableId ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart chartId (LineChart.toggleChartableVisible chartableId)
            in
            ( model
                |> setUserData userData_
                |> (updateChartModel chartId <| Graph.toggleDataSet chartableId)
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
                        updateSelectedDataSet
                            << (\c ->
                                    { c
                                        | editState =
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
                        updateSelectedDataSet
                            << (\c ->
                                    { c
                                        | editState = AddingChartable chartableId
                                    }
                               )
                   )
            , Cmd.none
            )

        ChartableAddConfirmClicked chartId ->
            let
                ( chartableIdM, userData_, isNew ) =
                    case model.charts |> Listx.lookup chartId |> Maybe.map .editState of
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
                                updateSelectedDataSet
                                    << (\c ->
                                            { c
                                                | data = ( chartableId, newChartableModel ) :: c.data
                                                , editState =
                                                    if isNew then
                                                        EditingChartable chartableId True

                                                    else
                                                        NotEditing
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
                        updateSelectedDataSet
                            << (\c ->
                                    { c
                                        | editState = NotEditing
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
                        \c ->
                            { c
                                | data = c.data |> List.filter (\( cId, _ ) -> cId /= chartableId)
                                , editState = NotEditing
                            }
                   )
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        TrackableChanged chartId chartableId trackableId (Just newTrackableId) ->
            let
                questionM =
                    model.trackableOptions |> Listx.lookup newTrackableId

                userData_ =
                    model.userData |> UserData.updateChartable chartableId (Chartable.replaceTrackable trackableId newTrackableId)

                chartableM_ =
                    userData_ |> UserData.getChartable chartableId
            in
            case ( questionM, chartableM_ ) of
                ( Just question, Just chartable_ ) ->
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
                                    |> List.map Tuple.first
                                    |> List.filter (\tId -> not (List.member tId tIds))
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

        GraphMsg chartId graphMsg ->
            ( model |> (updateChartModel chartId <| Graph.update graphMsg), Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "bg-white" ] <|
        h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
            :: (model.charts
                    |> List.map (viewLineChart model.chartableOptions model.trackableOptions)
               )


viewLineChart : List ( ChartableId, String ) -> List ( TrackableId, String ) -> ( LineChartId, Graph.Model LineChartModel ChartableId ChartableModel ) -> Html Msg
viewLineChart chartableOptions trackableOptions ( chartId, model ) =
    let
        viewChartable ( chartableId, dataSet ) =
            let
                canMoveUp =
                    (Maybe.map Tuple.first << List.head) model.data /= Just chartableId

                canMoveDown =
                    (Maybe.map Tuple.first << List.head << List.reverse) model.data /= Just chartableId
            in
            [ div
                [ class "border-t-4"
                , Colour.class "bg" dataSet.colour
                , Colour.classUp "border" dataSet.colour
                , classList
                    [ ( "grayscale", not dataSet.visible )
                    ]
                , onMouseEnter <| ChartableHovered chartId (Just chartableId)
                , onMouseLeave <| ChartableHovered chartId Nothing
                ]
                [ if model.editState == EditingChartable chartableId True || model.editState == EditingChartable chartableId False then
                    div
                        [ class "px-4 py-2 flex items-center"
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
                            , onClick (ChartableCloseClicked chartId)
                            ]
                            [ icon "w-5 h-5" <| SolidTimes ]
                        ]

                  else
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
                ]
            , if model.editState == EditingChartable chartableId True || model.editState == EditingChartable chartableId False then
                div
                    [ class "p-4"
                    , Colour.classDown "bg" dataSet.colour
                    , classList
                        [ ( "grayscale", not dataSet.visible )
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
                                        TrackableId.toString
                                        TrackableId.fromString
                                        (trackableOptions
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
    in
    div []
        [ div [ class "mx-4 my-0 flex scrollable-parent", style "height" "300px" ]
            [ viewJustYAxis "flex-grow-0 flex-shrink-0" model
            , viewScrollableContainer ("chart" ++ LineChartId.toString chartId) [ Html.map (GraphMsg chartId) <| viewLineGraph "h-full" model ]
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
            (case model.editState of
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
                            , onClick (ChartableAddCancelClicked chartId)
                            ]
                            [ icon "w-5 h-5" <| SolidTimes ]
                        ]

                EditingChartable _ True ->
                    div [] []

                _ ->
                    div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                        [ Controls.button "" Controls.ButtonGrey (ChartableAddClicked chartId) SolidPlusCircle "Add chartable" True
                        ]
            )
                :: (model.data |> List.concatMap viewChartable)
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
