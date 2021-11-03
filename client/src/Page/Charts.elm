module Page.Charts exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation
import Chart.LineChart as Chart
import Controls
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Htmlx
import IdDict
import Listx
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.ChartableId exposing (ChartableId)
import UserData.LineChart exposing (LineChart)
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable exposing (TrackableData(..))


type alias Model =
    { today : Date
    , charts : List ( LineChartId, Chart.Model )
    , chartableOptions : List ( ChartableId, String )
    , userData : UserData
    }


init : Date -> UserData -> ( Model, Cmd Msg )
init today userData =
    let
        chartsWithCmds =
            UserData.activeLineCharts userData
                |> Listx.mapLookup (toChartModel today userData)
    in
    ( { today = today
      , charts = chartsWithCmds |> List.map (\( id, ( chart, _ ) ) -> ( id, chart ))
      , chartableOptions =
            UserData.chartables userData
                |> IdDict.toList
                |> (List.map <| Tuple.mapSecond (Stringx.withDefault "[no name]" << .name))
                |> List.sortBy (String.toUpper << Tuple.second)
      , userData = userData
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
    model.charts |> List.map (\( id, chart ) -> Sub.map (ChartMsg id) (Chart.subscriptions chart)) |> Sub.batch



-- UPDATE


type Msg
    = ChartAddClicked
    | ChartDeleteClicked LineChartId
    | ChartUpClicked LineChartId
    | ChartDownClicked LineChartId
    | UserDataUpdated UserData
    | ChartMsg LineChartId Chart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChart chartId fn m =
            { m | charts = m.charts |> Listx.updateLookup chartId fn }

        addChart chartId chart m =
            { m | charts = m.charts |> Listx.insertLookup chartId chart }

        setUserData userData_ m =
            { m | userData = userData_ }
    in
    case msg of
        ChartMsg chartId chartMsg ->
            let
                chartM =
                    model.charts |> Listx.lookup chartId
            in
            case chartM of
                Just chart ->
                    let
                        ( chart_, cmd ) =
                            Chart.update chartMsg chart
                    in
                    ( model |> updateChart chartId (always chart_), Cmd.map (ChartMsg chartId) cmd )

                _ ->
                    ( model, Cmd.none )

        ChartAddClicked ->
            let
                newChart =
                    { name = ""
                    , fillLines = True
                    , chartables = []
                    }

                ( idM, userData_ ) =
                    model.userData |> UserData.addLineChart newChart

                newChartModelM =
                    idM |> Maybe.map (\id -> Chart.init model.today model.userData id newChart)
            in
            case ( idM, newChartModelM ) of
                ( Just id, Just ( newChartModel, cmd ) ) ->
                    ( model
                        |> setUserData userData_
                        |> addChart id newChartModel
                    , Cmd.batch
                        [ Task.perform UserDataUpdated <| Task.succeed userData_

                        -- , Browser.Navigation.load ("/charts/" ++ LineChartId.toString id)
                        , Cmd.map (ChartMsg id) cmd
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        ChartDeleteClicked chartId ->
            let
                userData_ =
                    model.userData |> UserData.deleteLineChart chartId
            in
            ( model
                |> setUserData userData_
                |> (\m -> { m | charts = m.charts |> List.filter (\( id, _ ) -> id /= chartId) })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartUpClicked chartId ->
            let
                userData_ =
                    model.userData |> UserData.moveLineChartUp chartId
            in
            ( model
                |> setUserData userData_
                |> (\m -> { m | charts = m.charts |> Listx.moveHeadwardsBy Tuple.first chartId })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        ChartDownClicked chartId ->
            let
                userData_ =
                    model.userData |> UserData.moveLineChartDown chartId
            in
            ( model
                |> setUserData userData_
                |> (\m -> { m | charts = m.charts |> Listx.moveTailwardsBy Tuple.first chartId })
            , Task.perform UserDataUpdated <| Task.succeed userData_
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "bg-white"
        ]
        [ h2 [ class "py-4 font-bold text-2xl text-center" ]
            [ text "Charts" ]
        , div []
            (model.charts
                |> List.map
                    (\( chartId, chart ) ->
                        let
                            canMoveUp =
                                (Maybe.map Tuple.first << List.head) model.charts /= Just chartId

                            canMoveDown =
                                (Maybe.map Tuple.first << List.head << List.reverse) model.charts /= Just chartId
                        in
                        div
                            [ class "mt-12 first:mt-0" ]
                            [ div [ class "ml-8 mb-2 px-4 flex items-center" ]
                                [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black", href <| "/charts/" ++ LineChartId.toString chartId ]
                                    [ span [] [ text <| Stringx.withDefault "[no name]" chart.name ]
                                    , icon "absolute right-0 w-5 h-5" SolidPencilAlt
                                    ]
                                , button
                                    [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                    , Htmlx.onClickStopPropagation (ChartDeleteClicked chartId)
                                    ]
                                    [ icon "w-5 h-5" <| SolidTrashAlt ]
                                , button
                                    [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                                    , classList
                                        [ ( "text-opacity-30 cursor-default", not canMoveUp )
                                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveUp )
                                        ]
                                    , Htmlx.onClickStopPropagation <| ChartUpClicked chartId
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
                                    , Htmlx.onClickStopPropagation <| ChartDownClicked chartId
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



--   ((
--             ))
-- ++ [
--    ]))
