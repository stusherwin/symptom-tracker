module Chartable exposing (Chartable, decode, encode)

import Colour exposing (Colour)
import Json.Decode as D
import Json.Encode as E


type alias Chartable =
    { name : String
    , colour : Colour
    , sum : List ( Int, Float )
    }


decode : D.Decoder Chartable
decode =
    D.map3 (\name colour sum -> { name = name, colour = colour, sum = sum })
        (D.field "name" D.string)
        (D.field "colour" Colour.decode)
        (D.field "sum" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "trackableId" D.int)
                    (D.field "multiplier" D.float)
        )


encode : Chartable -> E.Value
encode { name, colour, sum } =
    E.object
        [ ( "name", E.string name )
        , ( "colour", Colour.encode colour )
        , ( "sum"
          , E.list
                (\( trackableId, multiplier ) ->
                    E.object
                        [ ( "trackableId", E.int trackableId )
                        , ( "multiplier", E.float multiplier )
                        ]
                )
                sum
          )
        ]
