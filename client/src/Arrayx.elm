module Arrayx exposing (..)

import Array exposing (Array)


update : Int -> (a -> a) -> Array a -> Array a
update i fn array =
    case Array.get i array of
        Just x ->
            Array.set i (fn x) array

        _ ->
            array
