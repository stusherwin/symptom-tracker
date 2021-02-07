module UserData exposing (UserData, addTrackable, chartables, charts, decode, deleteTrackable, encode, init, trackables, updateTrackable)

import Array
import Chart exposing (Chart(..))
import Chartable exposing (Chartable)
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Icon exposing (IconType(..))
import Json.Decode as D
import Json.Encode as E
import Time exposing (Month(..))
import Trackable exposing (Trackable, TrackableData(..))


type UserData
    = UserData
        { trackables : Dict Int Trackable
        , chartables : Dict Int Chartable
        , charts : Dict Int Chart
        }


trackables : UserData -> Dict Int Trackable
trackables (UserData data) =
    data.trackables


chartables : UserData -> Dict Int Chartable
chartables (UserData data) =
    data.chartables


charts : UserData -> Dict Int Chart
charts (UserData data) =
    data.charts


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
        answersFromList : List ( Date, a ) -> Dict Int a
        answersFromList =
            Dict.fromList << List.map (Tuple.mapFirst Date.toRataDie)
    in
    UserData
        { trackables =
            Dict.fromList
                [ ( 1
                  , { question = "How did you feel?"
                    , colour = Red
                    , multiplier = 1.0
                    , data =
                        TIcon (Array.fromList [ SolidTired, SolidFrownOpen, SolidMeh, SolidGrin, SolidLaughBeam ]) <|
                            Dict.fromList
                                [ ( 737772, 3 ), ( 737773, 2 ), ( 737774, 3 ), ( 737789, 1 ), ( 737793, 1 ), ( 737811, 0 ), ( 737812, 1 ), ( 737813, 2 ), ( 737814, 4 ), ( 737815, 0 ), ( 737816, 2 ), ( 737817, 0 ), ( 737818, 3 ), ( 737819, 4 ), ( 737820, 2 ), ( 737821, 3 ), ( 737824, 0 ) ]
                    }
                  )
                , ( 2
                  , { question = "Did you have a bath?"
                    , colour = Green
                    , multiplier = 1.0
                    , data =
                        TYesNo <|
                            Dict.fromList
                                [ ( 737789, True ), ( 737814, True ), ( 737820, False ), ( 737824, True ) ]
                    }
                  )
                , ( 3
                  , { question = "Did you smoke?"
                    , colour = Orange
                    , multiplier = 1.0
                    , data =
                        TYesNo <|
                            Dict.fromList
                                [ ( 737772, True ), ( 737773, False ), ( 737789, False ), ( 737814, False ), ( 737820, True ), ( 737824, False ) ]
                    }
                  )
                , ( 4
                  , { question = "What was your energy level?"
                    , colour = Blue
                    , multiplier = 1.0
                    , data =
                        TScale 1
                            11
                        <|
                            Dict.fromList
                                [ ( 737789, 3 ), ( 737814, 1 ), ( 737816, 5 ), ( 737817, 11 ), ( 737818, 0 ), ( 737820, 5 ), ( 737824, 3 ) ]
                    }
                  )
                , ( 5
                  , { question = "How many chocolate bars did you eat?"
                    , colour = Pink
                    , multiplier = 1.0
                    , data =
                        TInt <|
                            Dict.fromList
                                [ ( 737789, 1 ), ( 737814, 1 ), ( 737820, 3 ), ( 737824, 2 ) ]
                    }
                  )
                , ( 6
                  , { question = "How many miles did you run?"
                    , colour = Purple
                    , multiplier = 1.0
                    , data =
                        TFloat <|
                            Dict.fromList
                                [ ( 737789, 7 ), ( 737814, 2.5 ), ( 737815, 10 ), ( 737816, 0 ), ( 737817, 12 ), ( 737818, 1 ), ( 737819, 11 ), ( 737820, 2 ), ( 737821, 1 ), ( 737822, 10 ), ( 737824, 2.3 ) ]
                    }
                  )
                , ( 7
                  , { question = "Any other notes?"
                    , colour = Rose
                    , multiplier = 1.0
                    , data =
                        TText <|
                            Dict.fromList
                                [ ( 737814, "fdsa" ), ( 737824, "xsdf" ) ]
                    }
                  )
                ]
        , chartables =
            Dict.fromList
                [ ( 1, { name = "Mood", colour = Colour.Fuchsia, inverted = False, sum = [ ( 1, 1.0 ) ] } )
                , ( 2, { name = "Bath", colour = Colour.Blue, inverted = False, sum = [ ( 2, 5.0 ) ] } )
                , ( 3, { name = "Bad things", colour = Colour.Orange, inverted = True, sum = [ ( 3, 5.0 ), ( 5, 1.0 ) ] } )
                , ( 4, { name = "Energy", colour = Colour.Green, inverted = False, sum = [ ( 4, 1.0 ) ] } )
                , ( 5, { name = "Running", colour = Colour.Indigo, inverted = False, sum = [ ( 6, 1.0 ) ] } )
                ]
        , charts =
            Dict.fromList
                [ ( 1
                  , LineChart
                        { name = "All Data"
                        , fillLines = True
                        , showPoints = False
                        , chartables = [ 1, 2, 3, 4, 5 ]
                        }
                  )
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

        v0 =
            D.map (\ts -> UserData { trackables = ts, chartables = Dict.empty, charts = Dict.empty })
                (dictInt Trackable.decode)

        v1 =
            D.map3 (\tbles cbles cts -> UserData { trackables = tbles, chartables = cbles, charts = cts })
                (D.field "trackables" <| dictInt Trackable.decode)
                (D.field "chartables" <| dictInt Chartable.decode)
                (D.field "charts" <| dictInt Chart.decode)
    in
    D.oneOf
        [ D.null init
        , v0
        , v1
        , D.field "version" D.int
            |> D.andThen
                (\version ->
                    case version of
                        1 ->
                            D.field "data" v1

                        v ->
                            D.fail <| "Unknown version " ++ String.fromInt v
                )
        ]


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
    E.object
        [ ( "version", E.int 1 )
        , ( "data"
          , E.object
                [ ( "trackables", dictInt Trackable.encode data.trackables )
                , ( "chartables", dictInt Chartable.encode data.chartables )
                , ( "charts", dictInt Chart.encode data.charts )
                ]
          )
        ]
