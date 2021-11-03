module UserData exposing (UserData, chartables, decode, encode, init, trackables, tryAddTrackable, tryDeleteTrackable, tryUpdateTrackable)

import Array
import Chart exposing (Chart, ChartDict)
import Chartable exposing (Chartable)
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Icon exposing (IconType(..))
import IdDict
import Json.Decode as D
import Json.Encode as E
import Time exposing (Month(..))
import Trackable exposing (Trackable, TrackableData(..), TrackableDict, TrackableId)


type UserData
    = UserData
        { trackables : TrackableDict
        , chartables : Dict Int Chartable
        , charts : ChartDict
        }


trackables : UserData -> TrackableDict
trackables (UserData data) =
    data.trackables


chartables : UserData -> Dict Int Chartable
chartables (UserData data) =
    data.chartables


charts : UserData -> ChartDict
charts (UserData data) =
    data.charts


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
            Trackable.toDict
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
        , chartables = Dict.empty
        , charts = Chart.toDict []
        }


tryUpdateTrackable : TrackableId -> (Trackable -> Result String Trackable) -> UserData -> Result String UserData
tryUpdateTrackable id fn (UserData data) =
    data.trackables |> IdDict.tryUpdate id fn |> Result.map (\updated -> UserData { data | trackables = updated })


tryAddTrackable : Trackable -> UserData -> Result String ( TrackableId, UserData )
tryAddTrackable trackable (UserData data) =
    data.trackables |> IdDict.tryAdd trackable |> Result.map (\( id, updated ) -> ( id, UserData { data | trackables = updated } ))


tryDeleteTrackable : TrackableId -> UserData -> Result String UserData
tryDeleteTrackable id (UserData data) =
    data.trackables |> IdDict.tryDelete id |> Result.map (\updated -> UserData { data | trackables = updated })


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
            D.map (\ts -> UserData { trackables = ts, chartables = Dict.empty, charts = Chart.toDict [] })
                Trackable.decodeDict

        v1 =
            D.map3 (\tbles cbles cts -> UserData { trackables = tbles, chartables = cbles, charts = cts })
                (D.field "trackables" <| Trackable.decodeDict)
                (D.field "chartables" <| dictInt Chartable.decode)
                (D.field "charts" <| Chart.decodeDict)
    in
    D.oneOf
        [ v0
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
                [ ( "trackables", Trackable.encodeDict data.trackables )
                , ( "chartables", dictInt Chartable.encode data.chartables )
                , ( "charts", Chart.encodeDict data.charts )
                ]
          )
        ]
