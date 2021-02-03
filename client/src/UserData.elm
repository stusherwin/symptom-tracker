module UserData exposing (UserData, addTrackable, decode, deleteTrackable, encode, init, trackables, updateTrackable)

import Array
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Icon exposing (IconType(..))
import Json.Decode as D
import Json.Encode as E
import Time exposing (Month(..))
import Trackable exposing (Trackable, TrackableData(..))


type alias Trackables a =
    { a | trackables : Dict Int Trackable }


type UserData
    = UserData (Trackables {})


trackables : UserData -> Dict Int Trackable
trackables (UserData data) =
    data.trackables


updateTrackable : Int -> (Trackable -> Result String Trackable) -> UserData -> Result String UserData
updateTrackable id update (UserData data) =
    case Dict.get id data.trackables of
        Nothing ->
            Err <| "Could not find trackable with id " ++ String.fromInt id

        Just trackable ->
            update trackable
                |> Result.map (\updated -> UserData { data | trackables = Dict.insert id updated data.trackables })


addTrackable : Int -> Trackable -> UserData -> Result String UserData
addTrackable id trackable (UserData data) =
    case Dict.get id data.trackables of
        Just _ ->
            Err <| "Trackable already exists with id " ++ String.fromInt id

        _ ->
            Ok <| UserData { data | trackables = Dict.insert id trackable data.trackables }


deleteTrackable : Int -> UserData -> Result String UserData
deleteTrackable id (UserData data) =
    case Dict.get id data.trackables of
        Nothing ->
            Err <| "Could not find trackable with id " ++ String.fromInt id

        Just _ ->
            Ok <| UserData { data | trackables = Dict.remove id data.trackables }


init : UserData
init =
    let
        fromList : List Trackable -> Dict Int Trackable
        fromList ts =
            Dict.fromList <| List.map2 Tuple.pair (List.range 1 (List.length ts)) ts

        answersFromList : List ( Date, a ) -> Dict Int a
        answersFromList =
            Dict.fromList << List.map (Tuple.mapFirst Date.toRataDie)
    in
    UserData
        { trackables =
            fromList
                [ { question = "How did you feel?"
                  , colour = Red
                  , multiplier = 1.0
                  , data =
                        TIcon (Array.fromList [ SolidTired, SolidFrownOpen, SolidMeh, SolidGrin, SolidLaughBeam ]) <|
                            answersFromList
                                [ ( Date.fromCalendarDate 2020 Dec 13, 3 )
                                , ( Date.fromCalendarDate 2020 Dec 14, 4 )
                                ]
                  }
                , { question = "Did you have a bath?", colour = Green, multiplier = 1.0, data = TYesNo Dict.empty }
                , { question = "Did you smoke?"
                  , colour = Orange
                  , multiplier = 1.0
                  , data =
                        TYesNo <|
                            answersFromList
                                [ ( Date.fromCalendarDate 2020 Dec 13, True )
                                , ( Date.fromCalendarDate 2020 Dec 14, False )
                                ]
                  }
                , { question = "What was your energy level?", colour = Blue, multiplier = 1.0, data = TScale 1 11 Dict.empty }
                , { question = "How many chocolate bars did you eat?", colour = Pink, multiplier = 1.0, data = TInt Dict.empty }
                , { question = "How many miles did you run?", colour = Purple, multiplier = 1.0, data = TFloat Dict.empty }
                , { question = "Any other notes?", colour = Rose, multiplier = 1.0, data = TText Dict.empty }
                ]
        }


decode : D.Decoder UserData
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
    D.map (\ts -> UserData { trackables = ts }) <| dictInt Trackable.decode


encode : UserData -> E.Value
encode (UserData data) =
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
    dictInt Trackable.encode data.trackables
