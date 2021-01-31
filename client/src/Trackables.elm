module Trackables exposing (Trackables, add, answersFromList, decode, delete, encode, fromList, init, map, toDict, toList, update)

import Array
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Icon exposing (IconType(..))
import Json.Decode as D
import Json.Encode as E
import Time exposing (Month(..))
import Trackable exposing (Trackable, TrackableData(..))


type Trackables
    = Trackables (Dict Int Trackable)


update : Int -> (Trackable -> Result String Trackable) -> Trackables -> Result String Trackables
update id fn (Trackables trackables) =
    let
        maybeT =
            Dict.get id trackables

        result =
            Maybe.map fn maybeT
    in
    case result of
        Nothing ->
            Err <| "Could not find trackable with id " ++ String.fromInt id

        Just (Err e) ->
            Err e

        Just (Ok updatedT) ->
            Ok <| Trackables <| Dict.insert id updatedT trackables


add : Int -> Trackable -> Trackables -> Result String Trackables
add id trackable (Trackables trackables) =
    case Dict.get id trackables of
        Just _ ->
            Err <| "Trackable already exists with id " ++ String.fromInt id

        _ ->
            Ok <| Trackables <| Dict.insert id trackable trackables


delete : Int -> Trackables -> Result String Trackables
delete id (Trackables trackables) =
    case Dict.get id trackables of
        Just _ ->
            Ok <| Trackables <| Dict.remove id trackables

        _ ->
            Err <| "Could not find trackable with id " ++ String.fromInt id


init : Trackables
init =
    fromList
        [ { question = "How did you feel?"
          , colour = Red
          , multiplier = 1.0
          , data =
                TIcon (Array.fromList [ SolidTired, SolidFrownOpen, SolidMeh, SolidGrin, SolidLaughBeam ]) <|
                    answersFromList
                        [ ( Date.fromCalendarDate 2020 Dec 13, 3 )
                        , ( Date.fromCalendarDate 2020 Dec 14, 4 )
                        ]
          }
        , { question = "Did you have a bath?", colour = Green, multiplier = 1.0, data = TYesNo Dict.empty }
        , { question = "Did you smoke?"
          , colour = Orange
          , multiplier = 1.0
          , data =
                TYesNo <|
                    answersFromList
                        [ ( Date.fromCalendarDate 2020 Dec 13, True )
                        , ( Date.fromCalendarDate 2020 Dec 14, False )
                        ]
          }
        , { question = "What was your energy level?", colour = Blue, multiplier = 1.0, data = TScale 1 11 Dict.empty }
        , { question = "How many chocolate bars did you eat?", colour = Pink, multiplier = 1.0, data = TInt Dict.empty }
        , { question = "How many miles did you run?", colour = Purple, multiplier = 1.0, data = TFloat Dict.empty }
        , { question = "Any other notes?", colour = Rose, multiplier = 1.0, data = TText Dict.empty }
        ]


map : (Trackable -> a) -> Trackables -> Dict Int a
map fn (Trackables trackables) =
    Dict.map (\_ t -> fn t) trackables


toList : Trackables -> List ( Int, Trackable )
toList (Trackables trackables) =
    Dict.toList trackables


toDict : Trackables -> Dict Int Trackable
toDict (Trackables trackables) =
    trackables


fromList : List Trackable -> Trackables
fromList ts =
    Trackables <| Dict.fromList <| List.map2 Tuple.pair (List.range 1 (List.length ts)) ts


answersFromList : List ( Date, a ) -> Dict Int a
answersFromList =
    Dict.fromList << List.map (Tuple.mapFirst Date.toRataDie)


decode : D.Decoder Trackables
decode =
    let
        listInt v =
            D.list <|
                D.map2 Tuple.pair
                    (D.field "id" D.int)
                    (D.field "value" v)

        dictInt v =
            D.map Dict.fromList (listInt v)
    in
    D.map Trackables <| dictInt Trackable.decode


encode : Trackables -> E.Value
encode (Trackables trackables) =
    let
        listInt f =
            E.list
                (\( id, v ) ->
                    E.object
                        [ ( "id", E.int id )
                        , ( "value", f v )
                        ]
                )

        dictInt f =
            listInt f
                << Dict.toList
    in
    dictInt Trackable.encode trackables
