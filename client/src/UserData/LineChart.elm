module UserData.LineChart exposing (LineChart, LineChartDict, addChartable, decode, deleteChartable, encode, moveChartableDown, moveChartableUp, setFillLines, setName, toggleChartableVisible)

import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import Listx
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChartId exposing (LineChartId)


type alias LineChart =
    { name : String
    , fillLines : Bool
    , chartables : List ( ChartableId, Bool )
    }


type alias LineChartDict =
    IdDict LineChartId LineChart


setName : String -> LineChart -> LineChart
setName name c =
    { c | name = name }


setFillLines : Bool -> LineChart -> LineChart
setFillLines fl c =
    { c | fillLines = fl }


addChartable : ChartableId -> LineChart -> LineChart
addChartable chartableId c =
    { c | chartables = c.chartables |> Listx.insertLookup chartableId True }


deleteChartable : ChartableId -> LineChart -> LineChart
deleteChartable chartableId c =
    { c | chartables = c.chartables |> List.filter (\( cId, _ ) -> cId /= chartableId) }


toggleChartableVisible : ChartableId -> LineChart -> LineChart
toggleChartableVisible chartableId c =
    { c | chartables = c.chartables |> Listx.updateLookup chartableId not }


moveChartableUp : ChartableId -> LineChart -> LineChart
moveChartableUp chartableId c =
    { c | chartables = c.chartables |> Listx.moveHeadwardsBy Tuple.first chartableId }


moveChartableDown : ChartableId -> LineChart -> LineChart
moveChartableDown chartableId c =
    { c | chartables = c.chartables |> Listx.moveTailwardsBy Tuple.first chartableId }


decode : D.Decoder LineChart
decode =
    D.map3
        (\name fillLines chartables ->
            { name = name
            , chartables = chartables
            , fillLines = fillLines
            }
        )
        (D.field "name" D.string)
        (D.field "fillLines" D.bool)
        (D.field "chartables" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "id" ChartableId.decode)
                    (D.field "visible" D.bool)
        )


encode : LineChart -> E.Value
encode c =
    E.object
        [ ( "name", E.string c.name )
        , ( "fillLines", E.bool c.fillLines )
        , ( "chartables"
          , c.chartables
                |> E.list
                    (\( id, visible ) ->
                        E.object
                            [ ( "id", ChartableId.encode id )
                            , ( "visible", E.bool visible )
                            ]
                    )
          )
        ]
