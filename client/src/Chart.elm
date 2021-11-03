module Chart exposing (Chart(..), ChartDict, ChartId(..), LineChartData, decode, decodeDict, encode, encodeDict, fromList, toDict)

import Chartable exposing (ChartableId)
import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E


type Chart
    = LineChart LineChartData


type alias LineChartData =
    { name : String
    , fillLines : Bool
    , showPoints : Bool
    , chartables : IdDict ChartableId { visible : Bool }
    , chartableOrder : List ChartableId
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
                D.map5
                    (\name fillLines showPoints chartables chartableOrder ->
                        { name = name
                        , chartables = chartables
                        , fillLines = fillLines
                        , showPoints = showPoints
                        , chartableOrder = chartableOrder
                        }
                    )
                    (D.field "name" D.string)
                    (D.field "fillLines" D.bool)
                    (D.field "showPoints" D.bool)
                    (D.field "chartables" <|
                        Chartable.decodeIdDict <|
                            D.map
                                (\v ->
                                    { visible = v }
                                )
                            <|
                                D.field "visible" D.bool
                    )
                    (D.field "chartableOrder" <|
                        D.list <|
                            Chartable.decodeId
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
                          , c.chartables
                                |> Chartable.encodeIdDict
                                    (\{ visible } ->
                                        E.object
                                            [ ( "visible", E.bool visible )
                                            ]
                                    )
                          )
                        , ( "chartableOrder"
                          , c.chartableOrder |> E.list Chartable.encodeId
                          )
                        ]
                  )
                ]
