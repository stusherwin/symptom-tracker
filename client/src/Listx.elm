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


lookup : id -> List ( id, a ) -> Maybe a
lookup id list =
    case list of
        [] ->
            Nothing

        ( xId, x ) :: xs ->
            if xId == id then
                Just x

            else
                lookup id xs


updateLookup : id -> (a -> a) -> List ( id, a ) -> List ( id, a )
updateLookup id fn list =
    case list of
        [] ->
            []

        ( xId, x ) :: xs ->
            if xId == id then
                ( xId, fn x ) :: xs

            else
                ( xId, x ) :: updateLookup id fn xs


replaceLookup : id -> a -> List ( id, a ) -> List ( id, a )
replaceLookup id newX list =
    case list of
        [] ->
            []

        ( xId, x ) :: xs ->
            if xId == id then
                ( xId, newX ) :: xs

            else
                ( xId, x ) :: replaceLookup id newX xs


updateLookupWithKey : id -> (( id, a ) -> ( id, a )) -> List ( id, a ) -> List ( id, a )
updateLookupWithKey id fn list =
    case list of
        [] ->
            []

        ( xId, x ) :: xs ->
            if xId == id then
                fn ( xId, x ) :: xs

            else
                ( xId, x ) :: updateLookupWithKey id fn xs
