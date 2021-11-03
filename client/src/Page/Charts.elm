module Page.Charts exposing (Model, Msg(..), init, subscriptions, update, urlChanged, view)

import Array
import Browser.Navigation as Nav
import Chart.LineChart as Chart
import Controls
import Date exposing (Date, Unit(..))
import Extra.Html exposing (..)
import Extra.List as List
import Html exposing (..)
import Html.Attributes exposing (..)
import IdDict
import Page.Chart as ChartPage
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as C
import UserData.ChartableId exposing (ChartableId)
import UserData.LineChart as LC exposing (LineChart(..))
import UserData.LineChartId as LCId exposing (LineChartId)
import UserData.Trackable exposing (Responses(..))


type alias Model =
    { today : Date
    , chartableOptions : List ( ChartableId, String )
    , state : State
    , navKey : Nav.Key
    }


type State
    = Charts (List ( LineChartId, Chart.Model ))
    | Chart ChartPage.Model


init : Date -> UserData -> Nav.Key -> Maybe LineChartId -> ( Model, Cmd Msg )
init today userData navKey chartIdM =
    case ( chartIdM, chartIdM |> Maybe.andThen (\chartId -> userData |> UserData.getLineChart chartId) ) of
        ( Just chartId, Just chart ) ->
            let
                ( chartModel, cmd ) =
                    ChartPage.init today userData chartId chart
            in
            ( { today = today
              , chartableOptions =
                    UserData.chartables userData
                        |> IdDict.toList
                        |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << C.name))
                        |> List.sortBy (String.toUpper << Tuple.second)
              , state = Chart chartModel
              , navKey = navKey
              }
            , Cmd.map ChartPageMsg cmd
            )

        _ ->
            let
                chartsWithCmds =
                    UserData.activeLineCharts userData
                        |> List.mapLookup (toChartModel today userData)
            in
            ( { today = today
              , chartableOptions =
                    UserData.chartables userData
                        |> IdDict.toList
                        |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << C.name))
                        |> List.sortBy (String.toUpper << Tuple.second)
              , state = Charts (chartsWithCmds |> List.map (\( id, ( chart, _ ) ) -> ( id, chart )))
              , navKey = navKey
              }
            , chartsWithCmds |> List.map (\( _, ( _, cmd ) ) -> cmd) |> Cmd.batch
            )


toChartModel : Date -> UserData -> LineChartId -> LineChart -> ( Chart.Model, Cmd Msg )
toChartModel today userData id chart =
    let
        ( chartModel, chartCmd ) =
            Chart.init today userData id chart
    in
    ( chartModel
    , Cmd.map (ChartMsg id) chartCmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Charts charts ->
            charts |> List.map (\( id, chart ) -> Sub.map (ChartMsg id) (Chart.subscriptions chart)) |> Sub.batch

        Chart chart ->
            Sub.map ChartPageMsg (ChartPage.subscriptions chart)



-- UPDATE


type Msg
    = NoOp
    | ChartAddClicked
    | ChartDeleteClicked LineChartId
    | ChartUpClicked LineChartId
    | ChartDownClicked LineChartId
    | UserDataUpdated UserData
    | ChartMsg LineChartId Chart.Msg
    | ChartPageMsg ChartPage.Msg


update : UserData -> Msg -> Model -> ( Model, Cmd Msg )
update userData msg model =
    let
        updateChart chartId fn =
            Tuple.mapFirst <|
                \m ->
                    { m
                        | state =
                            case m.state of
                                Charts charts ->
                                    Charts (charts |> List.updateLookup chartId fn)

                                Chart chart ->
                                    Chart { chart | chart = fn chart.chart }
                    }

        addChart chartId chart =
            Tuple.mapFirst <|
                \m ->
                    { m
                        | state =
                            case m.state of
                                Charts charts ->
                                    Charts (charts |> List.insertLookup chartId chart)

                                _ ->
                                    m.state
                    }

        deleteChart chartId =
            Tuple.mapFirst <|
                \m ->
                    { m
                        | state =
                            case m.state of
                                Charts charts ->
                                    Charts (charts |> List.filter (\( id, _ ) -> id /= chartId))

                                _ ->
                                    m.state
                    }

        moveChartUp chartId =
            Tuple.mapFirst <|
                \m ->
                    { m
                        | state =
                            case m.state of
                                Charts charts ->
                                    Charts (charts |> List.moveHeadwardsBy Tuple.first chartId)

                                _ ->
                                    m.state
                    }

        moveChartDown chartId =
            Tuple.mapFirst <|
                \m ->
                    { m
                        | state =
                            case m.state of
                                Charts charts ->
                                    Charts (charts |> List.moveTailwardsBy Tuple.first chartId)

                                _ ->
                                    m.state
                    }

        setUserData userData_ ( m, cmd ) =
            ( m
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )

        updateUserData fn ( m, cmd ) =
            let
                userData_ =
                    userData |> fn
            in
            ( m
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )
    in
    case msg of
        ChartMsg chartId chartMsg ->
            case model.state of
                Charts charts ->
                    case charts |> List.lookup chartId of
                        Just chart ->
                            let
                                ( chart_, cmd ) =
                                    Chart.update userData chartMsg chart
                            in
                            ( model, Cmd.none )
                                |> updateChart chartId (always chart_)
                                |> (Tuple.mapSecond <| \c -> Cmd.batch [ c, Cmd.map (ChartMsg chartId) cmd ])

                        _ ->
                            ( model, Cmd.none )

                Chart chart ->
                    let
                        ( chart_, cmd ) =
                            Chart.update userData chartMsg chart.chart
                    in
                    ( model, Cmd.none )
                        |> updateChart chart.chartId (always chart_)
                        |> (Tuple.mapSecond <| \c -> Cmd.batch [ c, Cmd.map (ChartMsg chart.chartId) cmd ])

        ChartPageMsg (ChartPage.UserDataUpdated userData_) ->
            ( model, Cmd.none )
                |> setUserData userData_

        ChartPageMsg chartPageMsg ->
            case model.state of
                Chart chartModel ->
                    let
                        ( chart_, cmd ) =
                            ChartPage.update userData chartPageMsg chartModel
                    in
                    ( model, Cmd.none )
                        |> (Tuple.mapFirst <| \m -> { m | state = Chart chart_ })
                        |> (Tuple.mapSecond <| \c -> Cmd.batch [ c, Cmd.map ChartPageMsg cmd ])

                _ ->
                    ( model, Cmd.none )

        ChartAddClicked ->
            let
                chartNames =
                    userData |> UserData.lineCharts |> IdDict.values |> List.map LC.name

                newChartState =
                    { name = Stringx.nextName chartNames "Line Chart " 1
                    , fillLines = True
                    , dataSets = Array.empty
                    }

                ( resultM, userData_ ) =
                    userData |> UserData.addLineChart newChartState
            in
            case resultM of
                Just ( newId, newChart ) ->
                    let
                        ( newChartModel, cmd ) =
                            Chart.init model.today userData_ newId newChart
                    in
                    ( model, Cmd.none )
                        |> setUserData userData_
                        |> addChart newId newChartModel
                        |> (Tuple.mapSecond <|
                                \c ->
                                    Cmd.batch
                                        [ c
                                        , Nav.pushUrl model.navKey ("/charts/" ++ LCId.toString newId)
                                        , Cmd.map (ChartMsg newId) cmd
                                        ]
                           )

                _ ->
                    ( model, Cmd.none )

        ChartDeleteClicked chartId ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.deleteLineChart chartId)
                |> deleteChart chartId

        ChartUpClicked chartId ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.moveLineChartUp chartId)
                |> moveChartUp chartId

        ChartDownClicked chartId ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.moveLineChartDown chartId)
                |> moveChartDown chartId

        _ ->
            ( model, Cmd.none )


urlChanged : UserData -> Maybe LineChartId -> Model -> ( Model, Cmd Msg )
urlChanged userData chartIdM model =
    case ( chartIdM, chartIdM |> Maybe.andThen (\chartId -> userData |> UserData.getLineChart chartId) ) of
        ( Just chartId, Just chart ) ->
            let
                ( chartModel, cmd ) =
                    ChartPage.init model.today userData chartId chart
            in
            ( { model
                | state = Chart chartModel
              }
            , Cmd.map ChartPageMsg cmd
            )

        _ ->
            let
                chartsWithCmds =
                    UserData.activeLineCharts userData
                        |> List.mapLookup (toChartModel model.today userData)
            in
            ( { model
                | state = Charts (chartsWithCmds |> List.map (\( id, ( chart, _ ) ) -> ( id, chart )))
              }
            , chartsWithCmds |> List.map (\( _, ( _, cmd ) ) -> cmd) |> Cmd.batch
            )



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Chart chart ->
            Html.map ChartPageMsg (ChartPage.view chart)

        Charts charts ->
            div
                [ class "bg-white"
                ]
                [ h2 [ class "py-4 font-bold text-2xl text-center" ]
                    [ text "Charts" ]
                , div []
                    (charts
                        |> List.map
                            (\( chartId, chart ) ->
                                let
                                    canMoveUp =
                                        (Maybe.map Tuple.first << List.head) charts /= Just chartId

                                    canMoveDown =
                                        (Maybe.map Tuple.first << List.head << List.reverse) charts /= Just chartId
                                in
                                div
                                    [ class "mt-12 first:mt-0" ]
                                    [ div [ class "ml-8 mb-2 px-4 flex items-center" ]
                                        [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black", href <| "/charts/" ++ LCId.toString chartId ]
                                            [ span [] [ text <| Stringx.withDefault "[no name]" chart.name ]
                                            , icon "absolute right-0 w-5 h-5" SolidPencilAlt
                                            ]
                                        , button
                                            [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                            , onClickStopPropagation (ChartDeleteClicked chartId)
                                            ]
                                            [ icon "w-5 h-5" <| SolidTrashAlt ]
                                        , button
                                            [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                                            , classList
                                                [ ( "text-opacity-30 cursor-default", not canMoveUp )
                                                , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveUp )
                                                ]
                                            , onClickStopPropagation <| ChartUpClicked chartId
                                            , disabled (not canMoveUp)
                                            ]
                                            [ icon "w-5 h-5" <| SolidArrowUp
                                            ]
                                        , button
                                            [ class "ml-1 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                                            , classList
                                                [ ( "text-opacity-30 cursor-default", not canMoveDown )
                                                , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveDown )
                                                ]
                                            , onClickStopPropagation <| ChartDownClicked chartId
                                            , disabled (not canMoveDown)
                                            ]
                                            [ icon "w-5 h-5" <| SolidArrowDown
                                            ]
                                        ]
                                    , Html.map (ChartMsg chartId) (Chart.view chart)
                                    ]
                            )
                    )
                , div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                    [ Controls.button "" Controls.ButtonGrey ChartAddClicked SolidPlusCircle "Add new chart" True
                    ]
                ]
