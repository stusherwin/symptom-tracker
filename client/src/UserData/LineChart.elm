module UserData.LineChart exposing (LineChart, LineChartDict, decode, encode)

import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E
import UserData.Chartable
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChartId as LineChartId exposing (LineChartId)


type alias LineChart =
    { name : String
    , fillLines : Bool
    , showPoints : Bool
    , chartables : List ( ChartableId, Bool )
    }


type alias LineChartDict =
    IdDict LineChartId LineChart


decode : D.Decoder LineChart
decode =
    D.map4
        (\name fillLines showPoints chartables ->
            { name = name
            , chartables = chartables
            , fillLines = fillLines
            , showPoints = showPoints
            }
        )
        (D.field "name" D.string)
        (D.field "fillLines" D.bool)
        (D.field "showPoints" D.bool)
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
        , ( "showPoints", E.bool c.showPoints )
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
