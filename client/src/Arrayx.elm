module Arrayx exposing (..)

import Array exposing (Array)
import Html exposing (a)
import Listx


update : Int -> (a -> a) -> Array a -> Array a
update i fn array =
    case Array.get i array of
        Just x ->
            Array.set i (fn x) array

        _ ->
            array


delete : Int -> Array a -> Array a
delete i array =
    Array.append (Array.slice 0 i array) (Array.slice (i + 1) (Array.length array) array)


updateLookup : id -> (a -> a) -> Array ( id, a ) -> Array ( id, a )
updateLookup id fn =
    Array.fromList << Listx.updateLookup id fn << Array.toList


updateLookupWithKey : id -> (( id, a ) -> ( id, a )) -> Array ( id, a ) -> Array ( id, a )
updateLookupWithKey id fn =
    Array.fromList << Listx.updateLookupWithKey id fn << Array.toList


lookup : id -> Array ( id, a ) -> Maybe a
lookup id =
    Listx.lookup id << Array.toList
