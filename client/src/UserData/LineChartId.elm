module UserData.LineChartId exposing (LineChartId(..), decode, decodeDict, encode, encodeDict, fromString, toDict, toString)

import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E


type LineChartId
    = LineChartId Int


toInt : LineChartId -> Int
toInt (LineChartId id) =
    id


toString (LineChartId id) =
    String.fromInt id


fromString =
    Maybe.map LineChartId << String.toInt


encode : LineChartId -> E.Value
encode (LineChartId id) =
    E.int id


decode : D.Decoder LineChartId
decode =
    D.map LineChartId D.int


dictProps : IdDictProps LineChartId
dictProps =
    { name = "Line Chart", fromId = toInt, toId = LineChartId }


toDict : List ( LineChartId, a ) -> IdDict LineChartId a
toDict list =
    IdDict dictProps (Dict.fromList <| List.map (Tuple.mapFirst dictProps.fromId) list)


decodeDict : D.Decoder a -> D.Decoder (IdDict LineChartId a)
decodeDict =
    IdDict.decode dictProps


encodeDict : (a -> E.Value) -> IdDict LineChartId a -> E.Value
encodeDict =
    IdDict.encode
