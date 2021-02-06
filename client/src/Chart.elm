module Chart exposing (Chart, ChartId, decode, encode)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


type Chart
    = LineChart
        { name : String
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
                D.map2
                    (\name chartables -> { name = name, chartables = chartables })
                    (D.field "name" D.string)
                    (D.field "chartables" <|
                        D.list D.int
                    )
        ]


encode : Chart -> E.Value
encode chart =
    case chart of
        LineChart { name, chartables } ->
            E.object
                [ ( "lineChart"
                  , E.object
                        [ ( "name", E.string name )
                        , ( "chartables"
                          , E.list E.int chartables
                          )
                        ]
                  )
                ]
