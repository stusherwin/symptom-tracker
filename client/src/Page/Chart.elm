module Page.Chart exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Dom as Dom
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
    , trackableOptions : List ( TrackableId, ( String, Bool ) )
    , chartableOptions : List ( ChartableId, String )
    , addState : EditState
    , userData : UserData
    , chartables : List ( ChartableId, ChartableModel )
    }


type EditState
    = NotAdding
    | AddingChartable (Maybe ChartableId)


type alias ChartableModel =
    { name : String
    , colour : Colour
    , visible : Bool
    , inverted : Bool
    , nameIsPristine : Bool
    , trackables : List ( TrackableId, TrackableModel )
    }


type alias TrackableModel =
    { question : String
    , multiplier : String
    , isValid : Bool
    }


init : Date -> UserData -> LineChartId -> LineChart -> ( Model, Cmd Msg )
init today userData chartId chart =
    let
        ( chartModel, chartCmd ) =
            Chart.init today userData chartId chart
    in
    ( { chartId = chartId
      , chart = chartModel
      , trackableOptions =
            UserData.activeTrackables userData
                |> List.map (Tuple.mapSecond (Tuple.mapFirst .question))
                |> List.sortBy (String.toUpper << Tuple.first << Tuple.second)
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
                            |> Maybe.map (\c -> ( chartableId, toChartableModel userData visible c ))
                    )
      }
    , Cmd.map ChartMsg chartCmd
    )


toChartableModel : UserData -> Bool -> Chartable -> ChartableModel
toChartableModel userData visible chartable =
    { name = chartable.name
    , colour = UserData.getChartableColour userData chartable
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChartMsg (Chart.subscriptions model.chart)



-- UPDATE


type Msg
    = NoOp
    | ChartMsg Chart.Msg
    | ChartableHovered (Maybe ChartableId)
    | ChartableClicked ChartableId
    | ChartableEditClicked ChartableId
    | ChartableCloseClicked
    | ChartableVisibleClicked ChartableId
    | ChartableUpClicked ChartableId
    | ChartableDownClicked ChartableId
    | ChartableChanged ChartableId (Maybe ChartableId)
    | ChartableNameUpdated ChartableId String
    | ChartableColourUpdated ChartableId (Maybe Colour)
    | ChartableInvertedChanged ChartableId Bool
    | ChartableAddClicked
    | ChartableToAddChanged (Maybe ChartableId)
    | ChartableAddConfirmClicked
    | ChartableAddCancelClicked
    | ChartableDeleteClicked ChartableId
    | TrackableChanged ChartableId TrackableId (Maybe TrackableId)
    | TrackableMultiplierUpdated ChartableId TrackableId String
    | TrackableAddClicked ChartableId
    | TrackableDeleteClicked ChartableId TrackableId
    | UserDataUpdated UserData


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
        ChartableHovered chartableId ->
            ( model |> (updateChart <| Chart.hoverDataSet chartableId), Cmd.none )

        ChartableClicked chartableId ->
            ( model |> (updateChart <| Chart.toggleDataSetSelected chartableId), Cmd.none )

        ChartableEditClicked chartableId ->
            ( model |> (updateChart <| Chart.selectDataSet (Just chartableId))
            , Task.attempt (always NoOp) <| Dom.focus <| "chart" ++ LineChartId.toString model.chartId ++ "-chartable" ++ ChartableId.toString chartableId ++ "-name"
            )

        ChartableCloseClicked ->
            ( model
                |> (updateChart <| \c -> { c | expandedValue = False })
                |> (updateChart <| Chart.selectDataSet Nothing)
            , Cmd.none
            )

        ChartableVisibleClicked chartableId ->
            let
                userData_ =
                    model.userData |> UserData.updateLineChart model.chartId (LineChart.toggleChartableVisible chartableId)
            in
            ( model
                |> setUserData userData_
                |> (updateChart <| Chart.toggleDataSetVisible chartableId)
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartableUpClicked chartableId ->
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

        ChartableDownClicked chartableId ->
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

        ChartableNameUpdated chartableId name ->
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

        ChartableColourUpdated chartableId colour ->
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

        ChartableInvertedChanged chartableId inverted ->
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
                    chartableM |> Maybe.map (toChartableModel userData_ True)

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
                            Task.attempt (always NoOp) <| Dom.focus <| "chart" ++ LineChartId.toString model.chartId ++ "-chartable" ++ ChartableId.toString chartableId ++ "-name"

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

        ChartableDeleteClicked chartableId ->
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

        TrackableChanged chartableId trackableId (Just newTrackableId) ->
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

        TrackableMultiplierUpdated chartableId trackableId stringValue ->
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

        TrackableAddClicked chartableId ->
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

        TrackableDeleteClicked chartableId trackableId ->
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

        ChartMsg chartMsg ->
            let
                ( chart_, cmd ) =
                    Chart.update chartMsg model.chart
            in
            ( model |> updateChart (always chart_), Cmd.map ChartMsg cmd )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "bg-white" ]
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text <| Stringx.withDefault "[no name]" model.chart.name ]
        , viewLineChart model
        ]


viewLineChart : Model -> Html Msg
viewLineChart model =
    let
        viewChartable ( chartableId, dataSet ) =
            let
                canMoveUp =
                    (Maybe.map Tuple.first << List.head) model.chart.graph.data /= Just chartableId

                canMoveDown =
                    (Maybe.map Tuple.first << List.head << List.reverse) model.chart.graph.data /= Just chartableId

                canEditColour =
                    List.length dataSet.trackables > 1

                options =
                    model.trackableOptions
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
                    if not dataSet.visible || not ((model.chart.graph.selectedDataSet == Nothing && model.chart.graph.hoveredDataSet == Nothing) || model.chart.graph.selectedDataSet == Just chartableId || model.chart.graph.hoveredDataSet == Just chartableId) then
                        Colour.Gray

                    else
                        dataSet.colour
            in
            [ div
                [ class "border-t-4"
                , Colour.class "bg" colour
                , Colour.classUp "border" colour
                , onMouseEnter <| ChartableHovered (Just chartableId)
                , onMouseLeave <| ChartableHovered Nothing
                ]
                [ if model.chart.graph.selectedDataSet /= Just chartableId then
                    div
                        [ class "p-4 flex items-center"
                        ]
                        [ button
                            [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                            , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartableId
                            ]
                            [ icon "w-5 h-5" <|
                                if dataSet.visible then
                                    SolidEye

                                else
                                    SolidEyeSlash
                            ]
                        , if dataSet.visible then
                            span [ class "ml-4 w-full", Htmlx.onClickStopPropagation NoOp ]
                                [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black", href "#", target "_self", Htmlx.onClickPreventDefault (ChartableClicked chartableId) ]
                                    [ if model.chart.graph.selectedDataSet == Just chartableId then
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
                            , Htmlx.onClickStopPropagation (ChartableDeleteClicked chartableId)
                            ]
                            [ icon "w-5 h-5" <| SolidTrashAlt ]
                        , button
                            [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                            , classList
                                [ ( "text-opacity-0 cursor-default", not canMoveUp )
                                , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveUp )
                                ]
                            , Htmlx.onClickStopPropagation <| ChartableUpClicked chartableId
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
                            , Htmlx.onClickStopPropagation <| ChartableDownClicked chartableId
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
                            , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartableId
                            ]
                            [ icon "w-5 h-5" <|
                                if dataSet.visible then
                                    SolidEye

                                else
                                    SolidEyeSlash
                            ]
                        , Controls.textbox [ class "ml-4 w-72" ]
                            [ id <| "chart" ++ LineChartId.toString model.chartId ++ "-chartable" ++ ChartableId.toString chartableId ++ "-name"
                            , placeholder "Name"
                            ]
                            dataSet.name
                            { isValid = True, isRequired = True, isPristine = dataSet.nameIsPristine }
                            (ChartableNameUpdated chartableId)
                        , label [ class "ml-12 flex-shrink-0 flex-grow-0 font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                        , input
                            [ type_ "checkbox"
                            , id "inverted"
                            , class "ml-2 flex-shrink-0 flex-grow-0"
                            , onCheck (ChartableInvertedChanged chartableId)
                            , checked dataSet.inverted
                            ]
                            []
                        , if canEditColour then
                            Controls.colourDropdown "ml-4 flex-shrink-0 flex-grow-0" (ChartableColourUpdated chartableId) (Just dataSet.colour) { showFilled = False }

                          else
                            span [ class "ml-4" ] []
                        , button
                            [ class "ml-auto rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                            , Htmlx.onClickStopPropagation ChartableCloseClicked
                            ]
                            [ icon "w-5 h-5" <| SolidTimes ]
                        ]
                ]
            , if model.chart.graph.selectedDataSet == Just chartableId then
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
                                        (TrackableChanged chartableId trackableId)
                                        TrackableId.toString
                                        TrackableId.fromString
                                        (options |> List.map (\( ( tId, visible ), q ) -> ( ( tId, visible || tId == trackableId ), q )))
                                        Nothing
                                        (Just trackableId)
                                        { showFilled = False }
                                    , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                                    , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True, isPristine = False } (TrackableMultiplierUpdated chartableId trackableId)
                                    , button
                                        [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                        , Htmlx.onClickStopPropagation (TrackableDeleteClicked chartableId trackableId)
                                        ]
                                        [ icon "w-5 h-5" <| SolidTrashAlt ]
                                    ]
                            )
                    )
                        ++ [ div [ class "mt-4 first:mt-0 flex" ]
                                [ Controls.button "ml-9 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey (TrackableAddClicked chartableId) SolidPlusCircle "Add trackable" (options |> List.any (Tuple.second << Tuple.first)) ]
                           ]

              else
                div [] []
            ]
    in
    div
        []
        [ Html.map ChartMsg (Chart.view model.chart)
        , div [ class "mt-8 bg-gray-200" ] <|
            (model.chartables |> List.concatMap viewChartable)
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
                                ]
                   ]
        ]
