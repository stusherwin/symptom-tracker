module UserData.Chartable exposing (Chartable, ChartableDict, decode, encode)

import Colour exposing (Colour)
import Dict
import IdDict exposing (IdDict(..), IdDictProps)
import Json.Decode as D
import Json.Encode as E
import UserData.ChartableId exposing (ChartableId)
import UserData.Trackable
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Chartable =
    { name : String
    , colour : Maybe Colour
    , inverted : Bool
    , sum : List ( TrackableId, Float )
    }


type alias ChartableDict =
    IdDict ChartableId Chartable


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
                    (D.field "trackableId" TrackableId.decode)
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
                        [ ( "trackableId", TrackableId.encode trackableId )
                        , ( "multiplier", E.float multiplier )
                        ]
                )
                sum
          )
        ]
