module Chartable exposing (Chartable, ChartableDict, ChartableId(..), decode, decodeDict, decodeId, decodeIdDict, encode, encodeDict, encodeId, encodeIdDict, fromList, toDict)

import Colour exposing (Colour)
import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E
import Trackable exposing (TrackableId)


type alias Chartable =
    { name : String
    , colour : Colour
    , inverted : Bool
    , sum : List ( TrackableId, Float )
    }


type ChartableId
    = ChartableId Int


fromId : ChartableId -> Int
fromId (ChartableId id) =
    id


type alias ChartableDict =
    IdDict ChartableId Chartable


encodeId : ChartableId -> E.Value
encodeId (ChartableId id) =
    E.int id


decodeId : D.Decoder ChartableId
decodeId =
    D.map ChartableId D.int


dictProps : IdDictProps ChartableId
dictProps =
    { name = "Chartable", fromId = fromId, toId = ChartableId }


toDict : List Chartable -> ChartableDict
toDict chartables =
    IdDict dictProps <| Dict.fromList <| List.map2 Tuple.pair (List.range 1 (List.length chartables)) chartables


fromList : List ( ChartableId, a ) -> IdDict ChartableId a
fromList list =
    IdDict dictProps (Dict.fromList <| List.map (Tuple.mapFirst dictProps.fromId) list)


decodeDict : D.Decoder ChartableDict
decodeDict =
    IdDict.decode dictProps decode


encodeDict : ChartableDict -> E.Value
encodeDict =
    IdDict.encode encode


decodeIdDict : D.Decoder a -> D.Decoder (IdDict ChartableId a)
decodeIdDict =
    IdDict.decode dictProps


encodeIdDict : (a -> E.Value) -> IdDict ChartableId a -> E.Value
encodeIdDict =
    IdDict.encode


decode : D.Decoder Chartable
decode =
    D.map4
        (\name colour inverted sum ->
            { name = name
            , colour = colour
            , inverted = inverted
            , sum = sum
            }
        )
        (D.field "name" D.string)
        (D.field "colour" Colour.decode)
        (D.field "inverted" D.bool)
        (D.field "sum" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "trackableId" Trackable.decodeId)
                    (D.field "multiplier" D.float)
        )


encode : Chartable -> E.Value
encode { name, colour, inverted, sum } =
    E.object
        [ ( "name", E.string name )
        , ( "colour", Colour.encode colour )
        , ( "inverted", E.bool inverted )
        , ( "sum"
          , E.list
                (\( trackableId, multiplier ) ->
                    E.object
                        [ ( "trackableId", Trackable.encodeId trackableId )
                        , ( "multiplier", E.float multiplier )
                        ]
                )
                sum
          )
        ]
