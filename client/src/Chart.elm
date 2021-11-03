module Chart exposing (Chart, ChartDict, ChartId, decode, decodeDict, encode, encodeDict, toDict)

import Dict exposing (Dict)
import IdDict exposing (IdDict(..), IdDictProps)
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


type alias ChartDict =
    IdDict ChartId Chart


dictProps : IdDictProps ChartId
dictProps =
    { name = "Chart", fromId = fromChartId, toId = ChartId }


toDict : List Chart -> ChartDict
toDict charts =
    IdDict dictProps <| Dict.fromList <| List.map2 Tuple.pair (List.range 1 (List.length charts)) charts


decodeDict : D.Decoder ChartDict
decodeDict =
    IdDict.decode dictProps decode


encodeDict : ChartDict -> E.Value
encodeDict =
    IdDict.encode encode


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
