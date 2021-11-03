module Chart exposing (Chart(..), ChartDict, ChartId(..), decode, decodeDict, encode, encodeDict, fromList, toDict)

import Chartable exposing (ChartableId)
import Dict exposing (Dict)
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E


type Chart
    = LineChart
        { name : String
        , fillLines : Bool
        , showPoints : Bool
        , chartables : List ChartableId
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


fromList : List ( ChartId, Chart ) -> ChartDict
fromList list =
    IdDict dictProps (Dict.fromList <| List.map (Tuple.mapFirst dictProps.fromId) list)


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
                        D.list Chartable.decodeId
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
                          , E.list Chartable.encodeId c.chartables
                          )
                        ]
                  )
                ]
