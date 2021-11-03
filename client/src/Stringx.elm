module Stringx exposing (..)


withDefault : String -> String -> String
withDefault default string =
    if String.isEmpty string then
        default

    else
        string
