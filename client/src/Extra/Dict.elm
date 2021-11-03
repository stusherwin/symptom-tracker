module Extra.Dict exposing (..)

import Dict exposing (Dict)


concatMaybes : Dict comparable (Maybe a) -> Dict comparable a
concatMaybes =
    Dict.fromList
        << List.filterMap
            (\( k, maybeV ) ->
                case maybeV of
                    Just v ->
                        Just ( k, v )

                    _ ->
                        Nothing
            )
        << Dict.toList


mapMaybes : (a -> Maybe b) -> Dict comparable (Maybe a) -> Dict comparable (Maybe b)
mapMaybes fn =
    Dict.map (\_ v -> Maybe.andThen fn v)


unionWith : (v -> v -> v) -> Dict comparable v -> Dict comparable v -> Dict comparable v
unionWith fn d1 d2 =
    Dict.merge
        Dict.insert
        (\k v1 v2 -> Dict.insert k (fn v1 v2))
        Dict.insert
        d1
        d2
        Dict.empty
