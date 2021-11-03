module UserData.ChartableId exposing (ChartableId(..), decode, decodeDict, encode, encodeDict, fromString, toDict, toString)

import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E


type ChartableId
    = ChartableId Int


toInt : ChartableId -> Int
toInt (ChartableId id) =
    id


toString (ChartableId id) =
    String.fromInt id


fromString =
    Maybe.map ChartableId << String.toInt


encode : ChartableId -> E.Value
encode (ChartableId id) =
    E.int id


decode : D.Decoder ChartableId
decode =
    D.map ChartableId D.int


dictProps : IdDictProps ChartableId
dictProps =
    { name = "Chartable", fromId = toInt, toId = ChartableId }


toDict : List ( ChartableId, a ) -> IdDict ChartableId a
toDict list =
    IdDict dictProps (Dict.fromList <| List.map (Tuple.mapFirst dictProps.fromId) list)


decodeDict : D.Decoder a -> D.Decoder (IdDict ChartableId a)
decodeDict =
    IdDict.decode dictProps


encodeDict : (a -> E.Value) -> IdDict ChartableId a -> E.Value
encodeDict =
    IdDict.encode
