module Chart exposing (Chart(..), ChartId, decode, encode)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


type Chart
    = LineChart
        { name : String
        , fillLines : Bool
        , showPoints : Bool
        , chartables : List Int
        }


type ChartId
    = ChartId Int


fromChartId : ChartId -> Int
fromChartId (ChartId id) =
    id


decode : D.Decoder Chart
decode =
    D.oneOf
        [ D.map LineChart <|
            D.field "lineChart" <|
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
                        D.list D.int
                    )
        ]


encode : Chart -> E.Value
encode chart =
    case chart of
        LineChart c ->
            E.object
                [ ( "lineChart"
                  , E.object
                        [ ( "name", E.string c.name )
                        , ( "fillLines", E.bool c.fillLines )
                        , ( "showPoints", E.bool c.showPoints )
                        , ( "chartables"
                          , E.list E.int c.chartables
                          )
                        ]
                  )
                ]
