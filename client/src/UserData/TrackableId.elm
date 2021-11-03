module UserData.TrackableId exposing (TrackableId(..), decode, decodeDict, encode, encodeDict, fromString, toDict, toString)

import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E


type TrackableId
    = TrackableId Int


toInt : TrackableId -> Int
toInt (TrackableId id) =
    id


toString (TrackableId id) =
    String.fromInt id


fromString =
    Maybe.map TrackableId << String.toInt


encode : TrackableId -> E.Value
encode (TrackableId id) =
    E.int id


decode : D.Decoder TrackableId
decode =
    D.map TrackableId D.int


dictProps : IdDictProps TrackableId
dictProps =
    { name = "Trackable", fromId = toInt, toId = TrackableId }


toDict : List ( TrackableId, a ) -> IdDict TrackableId a
toDict list =
    IdDict dictProps (Dict.fromList <| List.map (Tuple.mapFirst dictProps.fromId) list)


decodeDict : D.Decoder a -> D.Decoder (IdDict TrackableId a)
decodeDict =
    IdDict.decode dictProps


encodeDict : (a -> E.Value) -> IdDict TrackableId a -> E.Value
encodeDict =
    IdDict.encode
