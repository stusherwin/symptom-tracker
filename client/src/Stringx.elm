module Stringx exposing (..)


withDefault : String -> String -> String
withDefault default string =
    if String.isEmpty string then
        default

    else
        string


nextName : List String -> String -> Int -> String
nextName names prefix i =
    let
        name =
            prefix ++ String.fromInt i
    in
    if List.member name names then
        nextName names prefix (i + 1)

    else
        name
