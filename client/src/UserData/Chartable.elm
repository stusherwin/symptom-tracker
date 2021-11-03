module UserData.Chartable exposing (Chartable, ChartableDict, New, add, addTrackable, buildDict, colour, decode, deleteTrackable, encode, isInverted, name, ownColour, replaceTrackable, setInverted, setMultiplier, setName, setOwnColour, sum, updateTrackable)

import Colour exposing (Colour)
import Dict exposing (Dict)
import Extra.Dict as Dict
import Extra.List as List
import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import Tuple
import UserData.ChartableId exposing (ChartableId)
import UserData.Trackable as T exposing (Trackable, TrackableDict)
import UserData.TrackableId as TId exposing (TrackableId)


type Chartable
    = Chartable Data


type alias Data =
    { name : String
    , ownColour : Maybe Colour
    , isInverted : Bool
    , sum : List ( TrackableId, ( Trackable, Float ) )
    , colour : Colour
    }


type alias State =
    { name : String
    , ownColour : Maybe Colour
    , isInverted : Bool
    , sum : List ( TrackableId, Float )
    }


type alias New =
    { name : String
    , ownColour : Maybe Colour
    , isInverted : Bool
    , sum : List ( TrackableId, Float )
    }


type alias ChartableDict =
    IdDict ChartableId Chartable


name : Chartable -> String
name (Chartable c) =
    c.name


ownColour : Chartable -> Maybe Colour
ownColour (Chartable c) =
    c.ownColour


isInverted : Chartable -> Bool
isInverted (Chartable c) =
    c.isInverted


sum : Chartable -> List ( TrackableId, ( Trackable, Float ) )
sum (Chartable c) =
    c.sum


colour : Chartable -> Colour
colour (Chartable c) =
    c.colour


add : TrackableDict -> New -> ChartableDict -> Result String ( ( ChartableId, Chartable ), ChartableDict )
add trackables c dict =
    let
        chartable =
            build trackables c
    in
    dict |> IdDict.add chartable |> Result.map (\( id, dict_ ) -> ( ( id, chartable ), dict_ ))


buildDict : TrackableDict -> IdDict ChartableId State -> ChartableDict
buildDict trackables states =
    IdDict.map (\_ s -> build trackables s) states


build : TrackableDict -> State -> Chartable
build trackables s =
    let
        sum_ =
            s.sum
                |> List.filterMap (\( id, m ) -> IdDict.get id trackables |> Maybe.map (\t -> ( id, ( t, m ) )))
    in
    Chartable
        { name = s.name
        , ownColour = s.ownColour
        , isInverted = s.isInverted
        , sum = sum_
        , colour = buildColour sum_ s.ownColour
        }


state : Chartable -> State
state (Chartable c) =
    let
        sum_ =
            c.sum
                |> List.map (\( id, ( _, m ) ) -> ( id, m ))
    in
    { name = c.name
    , ownColour = c.ownColour
    , isInverted = c.isInverted
    , sum = sum_
    }


buildColour : List ( TrackableId, ( Trackable, Float ) ) -> Maybe Colour -> Colour
buildColour s ownCol =
    case ( s, ownCol ) of
        ( [], _ ) ->
            Colour.Gray

        ( ( _, ( t, _ ) ) :: [], _ ) ->
            T.colour t

        ( _, Just col ) ->
            col

        ( ( _, ( t, _ ) ) :: _, _ ) ->
            T.colour t



-- buildDataPoints : List ( TrackableId, ( Trackable, Float ) ) -> Bool -> Dict Int Float
-- buildDataPoints s inv =
--     let
--         invert d =
--             case List.maximum <| Dict.values d of
--                 Just max ->
--                     d |> Dict.map (\_ v -> max - v)
--                 _ ->
--                     d
--     in
--     s
--         |> List.map
--             (\( _, ( trackable, multiplier ) ) ->
--                 trackable
--                     |> T.onlyFloatData
--                     |> Dict.map (\_ v -> v * multiplier)
--             )
--         |> List.foldl (Dict.unionWith (\v1 v2 -> v1 + v2)) Dict.empty
--         |> (if inv then
--                 invert
--             else
--                 identity
--            )


setName : String -> Chartable -> Result String Chartable
setName n (Chartable c) =
    Ok <| Chartable { c | name = n }


setOwnColour : Maybe Colour -> Chartable -> Result String Chartable
setOwnColour col (Chartable c) =
    Ok <| Chartable { c | ownColour = col, colour = buildColour c.sum col }


setInverted : Bool -> Chartable -> Result String Chartable
setInverted inv (Chartable c) =
    Ok <| Chartable { c | isInverted = inv }


addTrackable : TrackableId -> Trackable -> Float -> Chartable -> Result String Chartable
addTrackable trackableId trackable multiplier (Chartable c) =
    case c.sum |> List.lookup trackableId of
        Just _ ->
            Err <| "Trackable " ++ TId.toString trackableId ++ " already exists in Chartable"

        _ ->
            let
                sum_ =
                    c.sum ++ [ ( trackableId, ( trackable, multiplier ) ) ]
            in
            Ok <|
                Chartable
                    { c
                        | sum = sum_
                        , colour = buildColour sum_ c.ownColour
                    }


deleteTrackable : TrackableId -> Chartable -> Result String Chartable
deleteTrackable trackableId (Chartable c) =
    case c.sum |> List.lookup trackableId of
        Nothing ->
            Err <| "Trackable " ++ TId.toString trackableId ++ " does not exist in Chartable"

        Just _ ->
            let
                sum_ =
                    c.sum |> List.deleteBy Tuple.first trackableId
            in
            Ok <|
                Chartable
                    { c
                        | sum = sum_
                        , colour = buildColour sum_ c.ownColour
                    }


replaceTrackable : TrackableId -> TrackableId -> Trackable -> Chartable -> Result String Chartable
replaceTrackable oldTrackableId newTrackableId newTrackable (Chartable c) =
    case c.sum |> List.lookup oldTrackableId of
        Nothing ->
            Err <| "Trackable " ++ TId.toString oldTrackableId ++ " does not exist in Chartable"

        Just _ ->
            let
                sum_ =
                    c.sum |> List.updateLookupWithKey oldTrackableId (\( _, ( _, multiplier ) ) -> ( newTrackableId, ( newTrackable, multiplier ) ))
            in
            Ok <|
                Chartable
                    { c
                        | sum = sum_
                        , colour = buildColour sum_ c.ownColour
                    }


updateTrackable : TrackableId -> Trackable -> Chartable -> Result String Chartable
updateTrackable trackableId trackable (Chartable c) =
    case c.sum |> List.lookup trackableId of
        Nothing ->
            Err <| "Trackable " ++ TId.toString trackableId ++ " does not exist in Chartable"

        Just _ ->
            let
                sum_ =
                    c.sum |> List.updateLookup trackableId (Tuple.mapFirst (always trackable))
            in
            Ok <|
                Chartable
                    { c
                        | sum = sum_
                        , colour = buildColour sum_ c.ownColour
                    }


setMultiplier : TrackableId -> Float -> Chartable -> Result String Chartable
setMultiplier trackableId multiplier (Chartable c) =
    case c.sum |> List.lookup trackableId of
        Nothing ->
            Err <| "Trackable " ++ TId.toString trackableId ++ " does not exist in Chartable"

        Just _ ->
            let
                sum_ =
                    c.sum |> List.updateLookup trackableId (Tuple.mapSecond <| always multiplier)
            in
            Ok <|
                Chartable
                    { c
                        | sum = sum_
                    }


decode : D.Decoder State
decode =
    D.map4
        (\n c i s ->
            { name = n
            , ownColour = c
            , isInverted = i
            , sum = s
            }
        )
        (D.field "name" D.string)
        (D.field "colour" <| D.oneOf [ D.null Nothing, D.map Just Colour.decode ])
        (D.field "inverted" D.bool)
        (D.field "sum" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "trackableId" TId.decode)
                    (D.field "multiplier" D.float)
        )


encode : Chartable -> E.Value
encode chartable =
    let
        s =
            state chartable
    in
    E.object
        [ ( "name", E.string s.name )
        , ( "colour"
          , case s.ownColour of
                Just c ->
                    Colour.encode c

                _ ->
                    E.null
          )
        , ( "inverted", E.bool s.isInverted )
        , ( "sum"
          , E.list
                (\( trackableId, multiplier ) ->
                    E.object
                        [ ( "trackableId", TId.encode trackableId )
                        , ( "multiplier", E.float multiplier )
                        ]
                )
                s.sum
          )
        ]
