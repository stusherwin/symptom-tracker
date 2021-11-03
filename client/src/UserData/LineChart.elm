module UserData.LineChart exposing (LineChart, LineChartDict, LineChartId(..), decode, decodeDict, encode, encodeDict, fromList, idToString, toDict)

import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E
import UserData.Chartable as Chartable exposing (ChartableId)


type alias LineChart =
    { name : String
    , fillLines : Bool
    , showPoints : Bool
    , chartables : IdDict ChartableId { visible : Bool }
    , chartableOrder : List ChartableId
    }


type LineChartId
    = LineChartId Int


fromLineChartId : LineChartId -> Int
fromLineChartId (LineChartId id) =
    id


idToString (LineChartId id) =
    String.fromInt id


type alias LineChartDict =
    IdDict LineChartId LineChart


dictProps : IdDictProps LineChartId
dictProps =
    { name = "LineChart", fromId = fromLineChartId, toId = LineChartId }


toDict : List LineChart -> LineChartDict
toDict charts =
    IdDict dictProps <| Dict.fromList <| List.map2 Tuple.pair (List.range 1 (List.length charts)) charts


fromList : List ( LineChartId, LineChart ) -> LineChartDict
fromList list =
    IdDict dictProps (Dict.fromList <| List.map (Tuple.mapFirst dictProps.fromId) list)


decodeDict : D.Decoder LineChartDict
decodeDict =
    IdDict.decode dictProps decode


encodeDict : LineChartDict -> E.Value
encodeDict =
    IdDict.encode encode


decode : D.Decoder LineChart
decode =
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


encode : LineChart -> E.Value
encode c =
    E.object
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
