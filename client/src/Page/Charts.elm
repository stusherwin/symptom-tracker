module Page.Charts exposing (Model, Msg(..), init, subscriptions, update, view)

import Chart.LineChart as Chart
import Controls
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import IdDict
import Listx
import Maybe exposing (Maybe)
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable exposing (Chartable)
import UserData.ChartableId exposing (ChartableId)
import UserData.LineChart exposing (LineChart)
import UserData.LineChartId as LineChartId exposing (LineChartId)
import UserData.Trackable exposing (TrackableData(..))
import UserData.TrackableId exposing (TrackableId)


type alias Model =
    { charts : List ( LineChartId, ( Chart.Model, List ( ChartableId, ChartableModel ) ) )
    , trackableOptions : List ( TrackableId, ( String, Bool ) )
    , chartableOptions : List ( ChartableId, String )
    , userData : UserData
    }


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
    let
        chartsWithCmds =
            UserData.lineCharts userData
                |> IdDict.map (toChartModel today userData)
                |> IdDict.toList
    in
    ( { charts = chartsWithCmds |> List.map (\( id, ( chart, _ ) ) -> ( id, chart ))
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
      }
    , chartsWithCmds |> List.map (\( _, ( _, cmd ) ) -> cmd) |> Cmd.batch
    )


toChartModel : Date -> UserData -> LineChartId -> LineChart -> ( ( Chart.Model, List ( ChartableId, ChartableModel ) ), Cmd Msg )
toChartModel today userData id chart =
    let
        toChartableModel_ ( chartableId, _ ) =
            userData
                |> UserData.getChartable chartableId
                |> Maybe.map (\c -> ( chartableId, toChartableModel userData c ))

        ( chartModel, chartCmd ) =
            Chart.init today userData id chart
    in
    ( ( chartModel
      , chart.chartables
            |> List.filterMap toChartableModel_
      )
    , Cmd.map (ChartMsg id) chartCmd
    )


toChartableModel : UserData -> Chartable -> ChartableModel
toChartableModel userData chartable =
    { inverted = chartable.inverted
    , trackables = chartable.sum |> List.filterMap (toTrackableModel userData)
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
    model.charts |> List.map (\( id, ( chart, _ ) ) -> Sub.map (ChartMsg id) (Chart.subscriptions chart)) |> Sub.batch



-- UPDATE


type Msg
    = ChartAddClicked
    | UserDataUpdated UserData
    | ChartMsg LineChartId Chart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateChart chartId fn m =
            { m | charts = m.charts |> Listx.updateLookup chartId (Tuple.mapFirst fn) }
    in
    case msg of
        ChartMsg chartId chartMsg ->
            let
                chartM =
                    model.charts |> Listx.lookup chartId |> Maybe.map Tuple.first
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

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "bg-white"
        ]
    <|
        h2 [ class "mb-4 py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
            :: (model.charts
                    |> List.map
                        (\( chartId, ( c, _ ) ) ->
                            div
                                []
                                [ div [ class "ml-8 mb-2 px-4" ]
                                    [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black", href <| "/charts/" ++ LineChartId.toString c.chartId ]
                                        [ span [] [ text <| c.name ]
                                        , icon "absolute right-4 w-5 h-5" SolidPencilAlt
                                        ]
                                    ]
                                , Html.map (ChartMsg chartId) (Chart.view model.chartableOptions model.userData c)
                                ]
                        )
               )
            ++ [ div [ class "px-4 py-2 bg-gray-300 border-t-4 border-gray-400 flex" ]
                    [ Controls.button "" Controls.ButtonGrey ChartAddClicked SolidPlusCircle "Add new chart" True
                    ]
               ]
