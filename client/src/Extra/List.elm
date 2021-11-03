module Extra.List exposing (..)


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


moveHeadwardsBy : (a -> b) -> b -> List a -> List a
moveHeadwardsBy fn item list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            if fn x == item then
                x :: y :: rest

            else if fn y == item then
                y :: x :: rest

            else
                x :: moveHeadwardsBy fn item (y :: rest)


moveTailwardsBy : (a -> b) -> b -> List a -> List a
moveTailwardsBy fn item list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            if fn x == item then
                y :: x :: rest

            else
                x :: moveTailwardsBy fn item (y :: rest)


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


findBy : (a -> b) -> b -> List a -> Maybe a
findBy fn id list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if fn x == id then
                Just x

            else
                findBy fn id xs


deleteLookup : id -> List ( id, a ) -> List ( id, a )
deleteLookup id list =
    List.filter (\( i, _ ) -> i /= id) list


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


mapLookup : (id -> a -> b) -> List ( id, a ) -> List ( id, b )
mapLookup fn =
    List.map (\( id, x ) -> ( id, fn id x ))


insertLookup : id -> a -> List ( id, a ) -> List ( id, a )
insertLookup id newX list =
    case list of
        [] ->
            [ ( id, newX ) ]

        ( xId, x ) :: xs ->
            if xId == id then
                ( xId, newX ) :: xs

            else
                ( xId, x ) :: insertLookup id newX xs


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


deleteBy : (a -> b) -> b -> List a -> List a
deleteBy fn id list =
    list |> List.filter (\x -> fn x /= id)
