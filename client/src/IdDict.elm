module IdDict exposing (IdDict(..), IdDictProps, add, concatMaybes, decode, delete, encode, filter, fromList, get, insert, keys, map, toDict, toList, {- tryAdd, tryDelete, tryUpdate, -} update, values)

import Dict exposing (Dict)
import Extra.Dict as Dict
import Json.Decode as D
import Json.Encode as E


type alias IdDictProps id =
    { name : String
    , fromId : id -> Int
    , toId : Int -> id
    }


type IdDict id entity
    = IdDict (IdDictProps id) (Dict Int entity)


get : id -> IdDict id entity -> Maybe entity
get id (IdDict { fromId } dict) =
    Dict.get (fromId id) dict


map : (id -> entity -> a) -> IdDict id entity -> IdDict id a
map fn (IdDict p dict) =
    IdDict p (Dict.map (\id e -> fn (p.toId id) e) dict)


filter : (id -> entity -> Bool) -> IdDict id entity -> IdDict id entity
filter fn (IdDict p dict) =
    IdDict p (Dict.filter (\id e -> fn (p.toId id) e) dict)


values : IdDict id entity -> List entity
values (IdDict _ dict) =
    Dict.values dict


keys : IdDict id entity -> List id
keys (IdDict p dict) =
    List.map p.toId <| Dict.keys dict


concatMaybes : IdDict id (Maybe a) -> IdDict id a
concatMaybes (IdDict p dict) =
    IdDict p (Dict.concatMaybes dict)


toDict : (id -> entity -> a) -> IdDict id entity -> Dict Int a
toDict fn (IdDict p dict) =
    Dict.map (\id e -> fn (p.toId id) e) dict


toList : IdDict id entity -> List ( id, entity )
toList (IdDict p dict) =
    Dict.toList dict |> List.map (Tuple.mapFirst p.toId)


update : id -> (entity -> Result String entity) -> IdDict id entity -> Result String (IdDict id entity)
update id updateEntity (IdDict e dict) =
    case Dict.get (e.fromId id) dict of
        Nothing ->
            Err <| "Could not find " ++ e.name ++ " with id " ++ String.fromInt (e.fromId id)

        Just entity ->
            updateEntity entity
                |> Result.map (\updated -> IdDict e <| Dict.insert (e.fromId id) updated dict)


add : entity -> IdDict id entity -> Result String ( id, IdDict id entity )
add entity (IdDict e dict) =
    let
        maxId =
            Maybe.withDefault 0 << List.maximum << Dict.keys <| dict

        newId =
            maxId + 1
    in
    Ok <| ( e.toId newId, IdDict e <| Dict.insert newId entity dict )


delete : id -> IdDict id entity -> Result String (IdDict id entity)
delete id (IdDict e dict) =
    case Dict.get (e.fromId id) dict of
        Nothing ->
            Err <| "Could not find " ++ e.name ++ " with id " ++ String.fromInt (e.fromId id)

        Just _ ->
            Ok <| IdDict e <| Dict.remove (e.fromId id) dict


-- update : id -> (entity -> entity) -> IdDict id entity -> IdDict id entity
-- update id updateEntity dict =
--     Result.withDefault dict <| tryUpdate id (Ok << updateEntity) dict


-- add : entity -> IdDict id entity -> ( Maybe id, IdDict id entity )
-- add entity dict =
--     Result.withDefault ( Nothing, dict ) <| Result.map (Tuple.mapFirst Just) <| tryAdd entity dict


-- delete : id -> IdDict id entity -> IdDict id entity
-- delete id dict =
--     Result.withDefault dict <| tryDelete id dict


insert : id -> entity -> IdDict id entity -> IdDict id entity
insert id entity (IdDict e dict) =
    IdDict e <| Dict.insert (e.fromId id) entity dict


decode : IdDictProps id -> D.Decoder entity -> D.Decoder (IdDict id entity)
decode e entityDecoder =
    D.map (IdDict e) <|
        D.map Dict.fromList <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "id" D.int)
                    (D.field "value" entityDecoder)


encode : (entity -> E.Value) -> IdDict id entity -> E.Value
encode encodeEntity (IdDict _ dict) =
    E.list
        (\( id, entity ) ->
            E.object
                [ ( "id", E.int id )
                , ( "value", encodeEntity entity )
                ]
        )
    <|
        Dict.toList dict


fromList : IdDictProps id -> List ( id, entity ) -> IdDict id entity
fromList p list =
    IdDict p (Dict.fromList <| List.map (Tuple.mapFirst p.fromId) list)
