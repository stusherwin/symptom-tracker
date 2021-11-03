module Listx exposing (..)


moveHeadwards : a -> List a -> List a
moveHeadwards item list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            if x == item then
                x :: y :: rest

            else if y == item then
                y :: x :: rest

            else
                x :: moveHeadwards item (y :: rest)


moveTailwards : a -> List a -> List a
moveTailwards item list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            if x == item then
                y :: x :: rest

            else
                x :: moveTailwards item (y :: rest)


concatMaybes : List (Maybe a) -> List a
concatMaybes list =
    case list of
        (Just x) :: xs ->
            x :: concatMaybes xs

        _ :: xs ->
            concatMaybes xs

        _ ->
            []
