module UserData.Trackable exposing (Trackable, TrackableData(..), TrackableDict, addIcon, convertToFloat, convertToIcon, convertToInt, convertToScale, convertToText, convertToYesNo, decode, deleteIcon, encode, hasData, maybeFloatData, onlyFloatData, setColour, setQuestion, textData, updateFloatData, updateIcon, updateIconData, updateIntData, updateScaleData, updateScaleFrom, updateScaleTo, updateTextData, updateYesNoData)

import Array exposing (Array)
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Dictx
import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import Svg.Icon as Icon exposing (IconType(..))
import Time exposing (Month(..))
import UserData.TrackableId exposing (TrackableId)


type alias Trackable =
    { question : String
    , colour : Colour
    , multiplier : Float
    , data : TrackableData
    }


type TrackableData
    = TYesNo (Dict Int Bool)
    | TIcon (Array IconType) (Dict Int Int)
    | TScale Int Int (Dict Int Int)
    | TInt (Dict Int Int)
    | TFloat (Dict Int Float)
    | TText (Dict Int String)


setColour : Colour -> Trackable -> Result String Trackable
setColour colour t =
    Ok { t | colour = colour }


setQuestion : String -> Trackable -> Result String Trackable
setQuestion question t =
    Ok { t | question = question }


maybeFloatData : Trackable -> Dict Int (Maybe Float)
maybeFloatData { data } =
    case data of
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
    Dictx.concatMaybes << maybeFloatData


textData : Trackable -> Dict Int String
textData { data } =
    case data of
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
hasData { data } =
    case data of
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
updateYesNoData day answer t =
    case t.data of
        TYesNo answers ->
            Ok
                { t
                    | data =
                        TYesNo <|
                            case answer of
                                Just ans ->
                                    Dict.insert (Date.toRataDie day) ans answers

                                _ ->
                                    Dict.remove (Date.toRataDie day) answers
                }

        _ ->
            Err <| "Could not update " ++ toString t.data ++ " trackable with yes/no data"


updateIcon : Int -> IconType -> Trackable -> Result String Trackable
updateIcon i iconType t =
    case t.data of
        TIcon options answers ->
            Ok
                { t
                    | data =
                        TIcon (Array.set i iconType options) answers
                }

        _ ->
            Err <| "Could not update icon for " ++ toString t.data ++ " trackable"


addIcon : IconType -> Trackable -> Result String Trackable
addIcon iconType t =
    case t.data of
        TIcon options answers ->
            Ok
                { t
                    | data =
                        TIcon (Array.push iconType options) answers
                }

        _ ->
            Err <| "Could not add icon to " ++ toString t.data ++ " trackable"


deleteIcon : Int -> Trackable -> Result String Trackable
deleteIcon i t =
    case t.data of
        TIcon options answers ->
            let
                newOptions =
                    Array.append (Array.slice 0 i options) (Array.slice (i + 1) (Array.length options) options)
            in
            Ok
                { t
                    | data =
                        TIcon newOptions answers
                }

        _ ->
            Err <| "Could not delete icon from " ++ toString t.data ++ " trackable"


updateIconData : Date -> Maybe Int -> Trackable -> Result String Trackable
updateIconData day answer t =
    case t.data of
        TIcon options answers ->
            Ok
                { t
                    | data =
                        TIcon options <|
                            case answer of
                                Just ans ->
                                    Dict.insert (Date.toRataDie day) ans answers

                                _ ->
                                    Dict.remove (Date.toRataDie day) answers
                }

        _ ->
            Err <| "Could not update " ++ toString t.data ++ " trackable with icon data"


updateScaleFrom : Int -> Trackable -> Result String Trackable
updateScaleFrom from t =
    case t.data of
        TScale _ to answers ->
            Ok { t | data = TScale from to answers }

        _ ->
            Err <| "Could not update 'scale from' value of " ++ toString t.data ++ " trackable"


updateScaleTo : Int -> Trackable -> Result String Trackable
updateScaleTo to t =
    case t.data of
        TScale from _ answers ->
            Ok { t | data = TScale from to answers }

        _ ->
            Err <| "Could not update 'scale to' value of " ++ toString t.data ++ " trackable"


updateScaleData : Date -> Maybe Int -> Trackable -> Result String Trackable
updateScaleData day answer t =
    case t.data of
        TScale min max answers ->
            case answer of
                Just ans ->
                    Ok { t | data = TScale min max <| Dict.insert (Date.toRataDie day) ans answers }

                _ ->
                    Ok { t | data = TScale min max <| Dict.remove (Date.toRataDie day) answers }

        _ ->
            Err <| "Could not update " ++ toString t.data ++ " trackable with scale data"


updateIntData : Date -> Maybe Int -> Trackable -> Result String Trackable
updateIntData day answer t =
    case t.data of
        TInt answers ->
            case answer of
                Just ans ->
                    Ok { t | data = TInt <| Dict.insert (Date.toRataDie day) ans answers }

                _ ->
                    Ok { t | data = TInt <| Dict.remove (Date.toRataDie day) answers }

        _ ->
            Err <| "Could not update " ++ toString t.data ++ " trackable with whole number data"


updateFloatData : Date -> Maybe Float -> Trackable -> Result String Trackable
updateFloatData day answer t =
    case t.data of
        TFloat answers ->
            case answer of
                Just ans ->
                    Ok { t | data = TFloat <| Dict.insert (Date.toRataDie day) ans answers }

                _ ->
                    Ok { t | data = TFloat <| Dict.remove (Date.toRataDie day) answers }

        _ ->
            Err <| "Could not update " ++ toString t.data ++ " trackable with decimal data"


updateTextData : Date -> String -> Trackable -> Result String Trackable
updateTextData day answer t =
    case t.data of
        TText answers ->
            Ok { t | data = TText <| Dict.insert (Date.toRataDie day) answer answers }

        _ ->
            Err <| "Could not update " ++ toString t.data ++ " trackable with text data"


convertToYesNo : Trackable -> Result String Trackable
convertToYesNo t =
    let
        data =
            maybeFloatData t

        converted =
            convert data

        convert =
            Dictx.concatMaybes
                << Dictx.mapMaybes
                    (\v ->
                        if v == 1 then
                            Just True

                        else if v == 0 then
                            Just False

                        else
                            Nothing
                    )
    in
    if Dict.size data == Dict.size converted then
        Ok { t | data = TYesNo converted }

    else
        Err <| "Could not convert all " ++ toString t.data ++ " trackable data to yes/no"


convertToIcon : Array IconType -> Trackable -> Result String Trackable
convertToIcon icons t =
    let
        data =
            maybeFloatData t

        converted =
            convert data

        convert =
            Dictx.concatMaybes
                << Dictx.mapMaybes
                    (\v ->
                        if ceiling v == floor v && v >= 0 && floor v <= Array.length icons then
                            Just (floor v)

                        else
                            Nothing
                    )
    in
    if Dict.size data == Dict.size converted then
        Ok { t | data = TIcon icons converted }

    else
        Err <| "Could not convert all " ++ toString t.data ++ " trackable data to icon"


convertToScale : Int -> Int -> Trackable -> Result String Trackable
convertToScale min max t =
    let
        data =
            maybeFloatData t

        converted =
            convert data

        convert =
            Dictx.concatMaybes
                << Dictx.mapMaybes
                    (\v ->
                        if ceiling v == floor v && floor v >= min && floor v <= max then
                            Just (floor v)

                        else
                            Nothing
                    )
    in
    if Dict.size data == Dict.size converted then
        Ok { t | data = TScale min max converted }

    else
        Err <| "Could not convert all " ++ toString t.data ++ " trackable data to scale"


convertToInt : Trackable -> Result String Trackable
convertToInt t =
    let
        data =
            maybeFloatData t

        converted =
            convert data

        convert =
            Dictx.concatMaybes
                << Dictx.mapMaybes
                    (\v ->
                        if ceiling v == floor v then
                            Just (floor v)

                        else
                            Nothing
                    )
    in
    if Dict.size data == Dict.size converted then
        Ok { t | data = TInt converted }

    else
        Err <| "Could not convert all " ++ toString t.data ++ " trackable data to whole number"


convertToFloat : Trackable -> Result String Trackable
convertToFloat t =
    let
        data =
            maybeFloatData t

        converted =
            convert data

        convert =
            Dictx.concatMaybes
                << Dictx.mapMaybes
                    (\v -> Just v)
    in
    if Dict.size data == Dict.size converted then
        Ok { t | data = TFloat converted }

    else
        Err <| "Could not convert all " ++ toString t.data ++ " trackable data to decimal"


convertToText : Trackable -> Result String Trackable
convertToText t =
    Ok { t | data = TText (textData t) }


toString : TrackableData -> String
toString data =
    case data of
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
    D.map4 Trackable
        (D.field "question" D.string)
        (D.field "colour" Colour.decode)
        (D.field "multiplier" D.float)
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
encode t =
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
        , ( "multiplier", E.float t.multiplier )
        , ( "data"
          , case t.data of
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
