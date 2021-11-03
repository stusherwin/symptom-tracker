module UserData.Chartable exposing (Chartable, ChartableDict, ChartableId(..), decode, decodeDict, decodeId, decodeIdDict, decodeList, encode, encodeDict, encodeId, encodeIdDict, encodeList, fromList, idFromString, idToString, toDict)

import Colour exposing (Colour)
import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E
import UserData.Trackable as Trackable exposing (TrackableId)


type alias Chartable =
    { name : String
    , colour : Maybe Colour
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


idToString (ChartableId id) =
    String.fromInt id


idFromString =
    Maybe.map ChartableId << String.toInt


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


decodeList : D.Decoder (List ( ChartableId, Chartable ))
decodeList =
    D.list <|
        D.map2 Tuple.pair
            (D.field "id" decodeId)
            (D.field "value" decode)


encodeDict : ChartableDict -> E.Value
encodeDict =
    IdDict.encode encode


encodeList : List ( ChartableId, Chartable ) -> E.Value
encodeList =
    E.list
        (\( id, entity ) ->
            E.object
                [ ( "id", encodeId id )
                , ( "value", encode entity )
                ]
        )


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
        (D.field "colour" <| D.oneOf [ D.null Nothing, D.map Just Colour.decode ])
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
        , ( "colour"
          , case colour of
                Just c ->
                    Colour.encode c

                _ ->
                    E.null
          )
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
