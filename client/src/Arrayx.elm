module Arrayx exposing (..)

import Array exposing (Array)
import Html exposing (a)


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
