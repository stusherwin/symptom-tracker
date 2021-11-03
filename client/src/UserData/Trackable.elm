module UserData.Trackable exposing (New, Responses(..), Trackable, TrackableDict, add, addIcon, colour, convertToFloat, convertToIcon, convertToInt, convertToScale, convertToText, convertToYesNo, decode, deleteIcon, encode, hasData, maybeFloatData, moveData, onlyFloatData, parseMultiplier, question, responses, setColour, setQuestion, textData, updateFloatData, updateIcon, updateIconData, updateIntData, updateScaleData, updateScaleFrom, updateScaleTo, updateTextData, updateYesNoData)

import Array exposing (Array)
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Extra.Dict as Dict
import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import String
import Svg.Icon as Icon exposing (IconType(..))
import Time exposing (Month(..))
import UserData.TrackableId exposing (TrackableId)


type Trackable
    = Trackable Data


type alias State =
    { question : String
    , colour : Colour
    , responses : Responses
    }


type alias Data =
    { question : String
    , colour : Colour
    , responses : Responses
    }


type alias New =
    { question : String
    , colour : Colour
    , responses : Responses
    }


type Responses
    = TYesNo (Dict Int Bool)
    | TIcon (Array IconType) (Dict Int Int)
    | TScale Int Int (Dict Int Int)
    | TInt (Dict Int Int)
    | TFloat (Dict Int Float)
    | TText (Dict Int String)


question : Trackable -> String
question (Trackable t) =
    t.question


colour : Trackable -> Colour
colour (Trackable t) =
    t.colour


responses : Trackable -> Responses
responses (Trackable t) =
    t.responses


add : New -> TrackableDict -> Result String ( ( TrackableId, Trackable ), TrackableDict )
add t dict =
    dict |> IdDict.add (Trackable t) |> Result.map (\( id, dict_ ) -> ( ( id, Trackable t ), dict_ ))


setColour : Colour -> Trackable -> Result String Trackable
setColour c (Trackable t) =
    Ok <| Trackable { t | colour = c }


setQuestion : String -> Trackable -> Result String Trackable
setQuestion q (Trackable t) =
    Ok <| Trackable { t | question = q }


maybeFloatData : Trackable -> Dict Int (Maybe Float)
maybeFloatData (Trackable t) =
    case t.responses of
        TYesNo values ->
            Dict.map
                (\_ v ->
                    if v then
                        Just 1.0

                    else
                        Just 0.0
                )
                values

        TIcon _ values ->
            Dict.map (\_ v -> Just (toFloat v)) values

        TScale _ _ values ->
            Dict.map (\_ v -> Just (toFloat v)) values

        TInt values ->
            Dict.map (\_ v -> Just (toFloat v)) values

        TFloat values ->
            Dict.map (\_ v -> Just v) values

        TText values ->
            Dict.map (\_ v -> String.toFloat v) values


onlyFloatData : Trackable -> Dict Int Float
onlyFloatData =
    Dict.concatMaybes << maybeFloatData


textData : Trackable -> Dict Int String
textData (Trackable t) =
    case t.responses of
        TYesNo values ->
            Dict.map
                (\_ v ->
                    if v then
                        "1"

                    else
                        "0"
                )
                values

        TIcon _ values ->
            Dict.map (\_ v -> String.fromInt v) values

        TScale _ _ values ->
            Dict.map (\_ v -> String.fromInt v) values

        TInt values ->
            Dict.map (\_ v -> String.fromInt v) values

        TFloat values ->
            Dict.map (\_ v -> String.fromFloat v) values

        TText values ->
            values


hasData : Trackable -> Bool
hasData (Trackable t) =
    case t.responses of
        TYesNo values ->
            not <| Dict.isEmpty values

        TIcon _ values ->
            not <| Dict.isEmpty values

        TScale _ _ values ->
            not <| Dict.isEmpty values

        TInt values ->
            not <| Dict.isEmpty values

        TFloat values ->
            not <| Dict.isEmpty values

        TText values ->
            not <| Dict.isEmpty values


updateYesNoData : Date -> Maybe Bool -> Trackable -> Result String Trackable
updateYesNoData day answer ((Trackable t) as trackable) =
    case t.responses of
        TYesNo answers ->
            Ok <|
                Trackable
                    { t
                        | responses =
                            TYesNo <|
                                case answer of
                                    Just ans ->
                                        Dict.insert (Date.toRataDie day) ans answers

                                    _ ->
                                        Dict.remove (Date.toRataDie day) answers
                    }

        _ ->
            Err <| "Could not update " ++ toString trackable ++ " with yes/no data"


updateIcon : Int -> IconType -> Trackable -> Result String Trackable
updateIcon i iconType ((Trackable t) as trackable) =
    case t.responses of
        TIcon options answers ->
            Ok <|
                Trackable
                    { t
                        | responses =
                            TIcon (Array.set i iconType options) answers
                    }

        _ ->
            Err <| "Could not update icon for " ++ toString trackable


addIcon : IconType -> Trackable -> Result String Trackable
addIcon iconType ((Trackable t) as trackable) =
    case t.responses of
        TIcon options answers ->
            Ok <|
                Trackable
                    { t
                        | responses =
                            TIcon (Array.push iconType options) answers
                    }

        _ ->
            Err <| "Could not add icon to " ++ toString trackable


deleteIcon : Int -> Trackable -> Result String Trackable
deleteIcon i ((Trackable t) as trackable) =
    case t.responses of
        TIcon options answers ->
            let
                newOptions =
                    Array.append (Array.slice 0 i options) (Array.slice (i + 1) (Array.length options) options)
            in
            Ok <|
                Trackable
                    { t
                        | responses =
                            TIcon newOptions answers
                    }

        _ ->
            Err <| "Could not delete icon from " ++ toString trackable


updateIconData : Date -> Maybe Int -> Trackable -> Result String Trackable
updateIconData day answer ((Trackable t) as trackable) =
    case t.responses of
        TIcon options answers ->
            Ok <|
                Trackable
                    { t
                        | responses =
                            TIcon options <|
                                case answer of
                                    Just ans ->
                                        Dict.insert (Date.toRataDie day) ans answers

                                    _ ->
                                        Dict.remove (Date.toRataDie day) answers
                    }

        _ ->
            Err <| "Could not update " ++ toString trackable ++ " with icon data"


updateScaleFrom : Int -> Trackable -> Result String Trackable
updateScaleFrom from ((Trackable t) as trackable) =
    case t.responses of
        TScale _ to answers ->
            Ok <| Trackable { t | responses = TScale from to answers }

        _ ->
            Err <| "Could not update 'scale from' value of " ++ toString trackable


updateScaleTo : Int -> Trackable -> Result String Trackable
updateScaleTo to ((Trackable t) as trackable) =
    case t.responses of
        TScale from _ answers ->
            Ok <| Trackable { t | responses = TScale from to answers }

        _ ->
            Err <| "Could not update 'scale to' value of " ++ toString trackable


updateScaleData : Date -> Maybe Int -> Trackable -> Result String Trackable
updateScaleData day answer ((Trackable t) as trackable) =
    case t.responses of
        TScale min max answers ->
            case answer of
                Just ans ->
                    Ok <| Trackable { t | responses = TScale min max <| Dict.insert (Date.toRataDie day) ans answers }

                _ ->
                    Ok <| Trackable { t | responses = TScale min max <| Dict.remove (Date.toRataDie day) answers }

        _ ->
            Err <| "Could not update " ++ toString trackable ++ " with scale data"


updateIntData : Date -> Maybe Int -> Trackable -> Result String Trackable
updateIntData day answer ((Trackable t) as trackable) =
    case t.responses of
        TInt answers ->
            case answer of
                Just ans ->
                    Ok <| Trackable { t | responses = TInt <| Dict.insert (Date.toRataDie day) ans answers }

                _ ->
                    Ok <| Trackable { t | responses = TInt <| Dict.remove (Date.toRataDie day) answers }

        _ ->
            Err <| "Could not update " ++ toString trackable ++ " with whole number data"


updateFloatData : Date -> Maybe Float -> Trackable -> Result String Trackable
updateFloatData day answer ((Trackable t) as trackable) =
    case t.responses of
        TFloat answers ->
            case answer of
                Just ans ->
                    Ok <| Trackable { t | responses = TFloat <| Dict.insert (Date.toRataDie day) ans answers }

                _ ->
                    Ok <| Trackable { t | responses = TFloat <| Dict.remove (Date.toRataDie day) answers }

        _ ->
            Err <| "Could not update " ++ toString trackable ++ " with decimal data"


updateTextData : Date -> String -> Trackable -> Result String Trackable
updateTextData day answer ((Trackable t) as trackable) =
    case t.responses of
        TText answers ->
            Ok <| Trackable { t | responses = TText <| Dict.insert (Date.toRataDie day) answer answers }

        _ ->
            Err <| "Could not update " ++ toString trackable ++ " with text data"


convertToYesNo : Trackable -> Result String Trackable
convertToYesNo ((Trackable t) as trackable) =
    let
        resps =
            maybeFloatData trackable

        converted =
            convert resps

        convert =
            Dict.concatMaybes
                << Dict.mapMaybes
                    (\v ->
                        if v == 1 then
                            Just True

                        else if v == 0 then
                            Just False

                        else
                            Nothing
                    )
    in
    if Dict.size resps == Dict.size converted then
        Ok <| Trackable { t | responses = TYesNo converted }

    else
        Err <| "Could not convert all " ++ toString trackable ++ " data to yes/no"


convertToIcon : Array IconType -> Trackable -> Result String Trackable
convertToIcon icons ((Trackable t) as trackable) =
    let
        resps =
            maybeFloatData trackable

        converted =
            convert resps

        convert =
            Dict.concatMaybes
                << Dict.mapMaybes
                    (\v ->
                        if ceiling v == floor v && v >= 0 && floor v <= Array.length icons then
                            Just (floor v)

                        else
                            Nothing
                    )
    in
    if Dict.size resps == Dict.size converted then
        Ok <| Trackable { t | responses = TIcon icons converted }

    else
        Err <| "Could not convert all " ++ toString trackable ++ " data to icon"


convertToScale : Int -> Int -> Trackable -> Result String Trackable
convertToScale min max ((Trackable t) as trackable) =
    let
        resps =
            maybeFloatData trackable

        converted =
            convert resps

        convert =
            Dict.concatMaybes
                << Dict.mapMaybes
                    (\v ->
                        if ceiling v == floor v && floor v >= min && floor v <= max then
                            Just (floor v)

                        else
                            Nothing
                    )
    in
    if Dict.size resps == Dict.size converted then
        Ok <| Trackable { t | responses = TScale min max converted }

    else
        Err <| "Could not convert all " ++ toString trackable ++ " data to scale"


convertToInt : Trackable -> Result String Trackable
convertToInt ((Trackable t) as trackable) =
    let
        resps =
            maybeFloatData trackable

        converted =
            convert resps

        convert =
            Dict.concatMaybes
                << Dict.mapMaybes
                    (\v ->
                        if ceiling v == floor v then
                            Just (floor v)

                        else
                            Nothing
                    )
    in
    if Dict.size resps == Dict.size converted then
        Ok <| Trackable { t | responses = TInt converted }

    else
        Err <| "Could not convert all " ++ toString trackable ++ " data to whole number"


convertToFloat : Trackable -> Result String Trackable
convertToFloat ((Trackable t) as trackable) =
    let
        resps =
            maybeFloatData trackable

        converted =
            convert resps

        convert =
            Dict.concatMaybes
                << Dict.mapMaybes
                    (\v -> Just v)
    in
    if Dict.size resps == Dict.size converted then
        Ok <| Trackable { t | responses = TFloat converted }

    else
        Err <| "Could not convert all " ++ toString trackable ++ " data to decimal"


convertToText : Trackable -> Result String Trackable
convertToText ((Trackable t) as trackable) =
    Ok <| Trackable { t | responses = TText (textData trackable) }


parseMultiplier : String -> Maybe Float
parseMultiplier stringValue =
    String.toFloat stringValue
        |> Maybe.andThen
            (\v ->
                if v > 0 then
                    Just v

                else
                    Nothing
            )



-- getDataPoints : Float -> Bool -> Trackable -> Dict Int Float
-- getDataPoints multiplier inverted trackable =
--     let
--         invert rs =
--             case List.maximum <| Dict.values rs of
--                 Just max ->
--                     rs |> Dict.map (\_ v -> max - v)
--                 _ ->
--                     rs
--     in
--     trackable
--         |> onlyFloatData
--         |> Dict.map (\_ v -> v * multiplier)
--         |> (if inverted then
--                 invert
--             else
--                 identity
--            )


moveData : Int -> Trackable -> Trackable
moveData days (Trackable t) =
    Trackable
        { t
            | responses =
                case t.responses of
                    TYesNo dict ->
                        TYesNo (dict |> Dict.toList |> List.map (\( d, v ) -> ( d |> Date.fromRataDie |> Date.add Days days |> Date.toRataDie, v )) |> Dict.fromList)

                    TIcon icons dict ->
                        TIcon icons (dict |> Dict.toList |> List.map (\( d, v ) -> ( d |> Date.fromRataDie |> Date.add Days days |> Date.toRataDie, v )) |> Dict.fromList)

                    TScale min max dict ->
                        TScale min max (dict |> Dict.toList |> List.map (\( d, v ) -> ( d |> Date.fromRataDie |> Date.add Days days |> Date.toRataDie, v )) |> Dict.fromList)

                    TInt dict ->
                        TInt (dict |> Dict.toList |> List.map (\( d, v ) -> ( d |> Date.fromRataDie |> Date.add Days days |> Date.toRataDie, v )) |> Dict.fromList)

                    TFloat dict ->
                        TFloat (dict |> Dict.toList |> List.map (\( d, v ) -> ( d |> Date.fromRataDie |> Date.add Days days |> Date.toRataDie, v )) |> Dict.fromList)

                    TText dict ->
                        TText (dict |> Dict.toList |> List.map (\( d, v ) -> ( d |> Date.fromRataDie |> Date.add Days days |> Date.toRataDie, v )) |> Dict.fromList)
        }


toString : Trackable -> String
toString (Trackable t) =
    let
        trackableType =
            case t.responses of
                TYesNo _ ->
                    "yes/no"

                TIcon _ _ ->
                    "icon"

                TScale _ _ _ ->
                    "scale"

                TInt _ ->
                    "whole number"

                TFloat _ ->
                    "decimal"

                TText _ ->
                    "text"
    in
    trackableType ++ " trackable"


type alias TrackableDict =
    IdDict TrackableId Trackable


decode : D.Decoder Trackable
decode =
    let
        listInt v =
            D.list <|
                D.map2 Tuple.pair
                    (D.field "id" D.int)
                    (D.field "value" v)

        dictInt v =
            D.map Dict.fromList (listInt v)
    in
    D.map Trackable <|
        D.map3 State
            (D.field "question" D.string)
            (D.field "colour" Colour.decode)
            (D.field "data" <|
                D.oneOf
                    [ D.field "yesNo" <| D.map TYesNo <| D.field "values" <| dictInt D.bool
                    , D.field "icon" <| D.map2 TIcon (D.field "options" <| D.array Icon.decode) (D.field "values" <| dictInt D.int)
                    , D.field "scale" <|
                        D.map3 TScale
                            (D.field "min" D.int)
                            (D.field "max" D.int)
                            (D.field "values" <| dictInt D.int)
                    , D.field "int" <| D.map TInt <| D.field "values" <| dictInt D.int
                    , D.field "float" <| D.map TFloat <| D.field "values" <| dictInt D.float
                    , D.field "text" <| D.map TText <| D.field "values" <| dictInt D.string
                    ]
            )


encode : Trackable -> E.Value
encode (Trackable t) =
    let
        listInt f =
            E.list
                (\( id, v ) ->
                    E.object
                        [ ( "id", E.int id )
                        , ( "value", f v )
                        ]
                )

        dictInt f =
            listInt f
                << Dict.toList
    in
    E.object
        [ ( "question", E.string t.question )
        , ( "colour", Colour.encode t.colour )
        , ( "data"
          , case t.responses of
                TYesNo vals ->
                    E.object
                        [ ( "yesNo"
                          , E.object
                                [ ( "values", dictInt E.bool vals )
                                ]
                          )
                        ]

                TIcon options vals ->
                    E.object
                        [ ( "icon"
                          , E.object
                                [ ( "options", E.array Icon.encode options )
                                , ( "values", dictInt E.int vals )
                                ]
                          )
                        ]

                TScale min max vals ->
                    E.object
                        [ ( "scale"
                          , E.object
                                [ ( "min", E.int min )
                                , ( "max", E.int max )
                                , ( "values", dictInt E.int vals )
                                ]
                          )
                        ]

                TInt vals ->
                    E.object
                        [ ( "int"
                          , E.object
                                [ ( "values", dictInt E.int vals )
                                ]
                          )
                        ]

                TFloat vals ->
                    E.object
                        [ ( "float"
                          , E.object
                                [ ( "values", dictInt E.float vals )
                                ]
                          )
                        ]

                TText vals ->
                    E.object
                        [ ( "text"
                          , E.object
                                [ ( "values", dictInt E.string vals )
                                ]
                          )
                        ]
          )
        ]
