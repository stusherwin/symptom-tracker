module Extra.Array exposing (..)

import Array exposing (Array)
import Extra.List as List
import Html exposing (a, i)
import Json.Decode exposing (array)


update : Int -> (a -> a) -> Array a -> Array a
update i fn array =
    case Array.get i array of
        Just x ->
            Array.set i (fn x) array

        _ ->
            array


swap : Int -> Int -> Array a -> Array a
swap i j array =
    let
        i_ =
            max 0 i

        j_ =
            min (Array.length array - 1) j
    in
    if i_ == j_ then
        array

    else
        case ( Array.get i_ array, Array.get j_ array ) of
            ( Just a, Just b ) ->
                array |> Array.set i_ b |> Array.set j_ a

            _ ->
                array


delete : Int -> Array a -> Array a
delete i array =
    Array.append (Array.slice 0 i array) (Array.slice (i + 1) (Array.length array) array)


updateLookup : id -> (a -> a) -> Array ( id, a ) -> Array ( id, a )
updateLookup id fn =
    Array.fromList << List.updateLookup id fn << Array.toList


updateLookupWithKey : id -> (( id, a ) -> ( id, a )) -> Array ( id, a ) -> Array ( id, a )
updateLookupWithKey id fn =
    Array.fromList << List.updateLookupWithKey id fn << Array.toList


lookup : id -> Array ( id, a ) -> Maybe a
lookup id =
    List.lookup id << Array.toList
