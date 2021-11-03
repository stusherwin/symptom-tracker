module UserData exposing (UserData, addChartable, chartables, decode, encode, getChartable, getLineChart, getTrackable, init, {- insertChartable, -} lineCharts, trackables, tryAddTrackable, tryDeleteTrackable, tryUpdateTrackable, updateChartable, updateLineChart)

import Array
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import IdDict
import Json.Decode as D
import Json.Encode as E
import Svg.Icon exposing (IconType(..))
import Time exposing (Month(..))
import UserData.Chartable as Chartable exposing (Chartable, ChartableDict)
import UserData.ChartableId as ChartableId exposing (ChartableId(..))
import UserData.LineChart as LineChart exposing (LineChart, LineChartDict)
import UserData.LineChartId as LineChartId exposing (LineChartId(..))
import UserData.Trackable as Trackable exposing (Trackable, TrackableData(..), TrackableDict)
import UserData.TrackableId as TrackableId exposing (TrackableId(..))


type UserData
    = UserData
        { trackables : TrackableDict
        , chartables : ChartableDict
        , lineCharts : LineChartDict
        }


trackables : UserData -> TrackableDict
trackables (UserData data) =
    data.trackables


getTrackable : TrackableId -> UserData -> Maybe Trackable
getTrackable id (UserData data) =
    IdDict.get id data.trackables


chartables : UserData -> ChartableDict
chartables (UserData data) =
    data.chartables


getChartable : ChartableId -> UserData -> Maybe Chartable
getChartable id (UserData data) =
    IdDict.get id data.chartables


lineCharts : UserData -> LineChartDict
lineCharts (UserData data) =
    data.lineCharts


getLineChart : LineChartId -> UserData -> Maybe LineChart
getLineChart id (UserData data) =
    IdDict.get id data.lineCharts


init : UserData
init =
    UserData
        { trackables =
            TrackableId.toDict
                [ ( TrackableId 1
                  , { question = "How did you feel?"
                    , colour = Red
                    , multiplier = 1.0
                    , data =
                        TIcon (Array.fromList [ SolidTired, SolidFrownOpen, SolidMeh, SolidGrin, SolidLaughBeam ]) <|
                            Dict.fromList
                                [ ( 737772, 3 ), ( 737773, 2 ), ( 737774, 3 ), ( 737789, 1 ), ( 737793, 1 ), ( 737811, 0 ), ( 737812, 1 ), ( 737813, 2 ), ( 737814, 4 ), ( 737815, 0 ), ( 737816, 2 ), ( 737817, 0 ), ( 737818, 3 ), ( 737819, 4 ), ( 737820, 2 ), ( 737821, 3 ), ( 737824, 0 ) ]
                    }
                  )
                , ( TrackableId 2
                  , { question = "Did you have a bath?"
                    , colour = Green
                    , multiplier = 1.0
                    , data =
                        TYesNo <|
                            Dict.fromList
                                [ ( 737789, True ), ( 737814, True ), ( 737820, False ), ( 737824, True ) ]
                    }
                  )
                , ( TrackableId 3
                  , { question = "Did you smoke?"
                    , colour = Orange
                    , multiplier = 1.0
                    , data =
                        TYesNo <|
                            Dict.fromList
                                [ ( 737772, True ), ( 737773, False ), ( 737789, False ), ( 737814, False ), ( 737820, True ), ( 737824, False ) ]
                    }
                  )
                , ( TrackableId 4
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
                , ( TrackableId 5
                  , { question = "How many chocolate bars did you eat?"
                    , colour = Pink
                    , multiplier = 1.0
                    , data =
                        TInt <|
                            Dict.fromList
                                [ ( 737789, 1 ), ( 737814, 1 ), ( 737820, 3 ), ( 737824, 2 ) ]
                    }
                  )
                , ( TrackableId 6
                  , { question = "How many miles did you run?"
                    , colour = Purple
                    , multiplier = 1.0
                    , data =
                        TFloat <|
                            Dict.fromList
                                [ ( 737789, 7 ), ( 737814, 2.5 ), ( 737815, 10 ), ( 737816, 0 ), ( 737817, 12 ), ( 737818, 1 ), ( 737819, 11 ), ( 737820, 2 ), ( 737821, 1 ), ( 737822, 10 ), ( 737824, 2.3 ) ]
                    }
                  )
                , ( TrackableId 7
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
            ChartableId.toDict
                [ ( ChartableId 1
                  , { name = "Mood"
                    , colour = Nothing
                    , inverted = False
                    , sum =
                        [ ( TrackableId 1, 1.0 )
                        ]
                    }
                  )
                , ( ChartableId 2
                  , { name = "Bath"
                    , colour = Nothing
                    , inverted = False
                    , sum =
                        [ ( TrackableId 2, 5.0 )
                        ]
                    }
                  )
                , ( ChartableId 3
                  , { name = "Bad things"
                    , colour = Just Colour.Orange
                    , inverted = True
                    , sum =
                        [ ( TrackableId 3, 5.0 )
                        , ( TrackableId 5, 1.0 )
                        ]
                    }
                  )
                , ( ChartableId 4
                  , { name = "Energy"
                    , colour = Nothing
                    , inverted = False
                    , sum =
                        [ ( TrackableId 4, 1.0 )
                        ]
                    }
                  )
                , ( ChartableId 5
                  , { name = "Running"
                    , colour = Nothing
                    , inverted = False
                    , sum =
                        [ ( TrackableId 6, 1.0 )
                        ]
                    }
                  )
                ]
        , lineCharts =
            LineChartId.toDict
                [ ( LineChartId 1
                  , { name = "All Data"
                    , fillLines = True
                    , showPoints = False
                    , chartables =
                        [ ( ChartableId 1, True )
                        , ( ChartableId 2, False )
                        , ( ChartableId 3, True )
                        , ( ChartableId 4, True )
                        , ( ChartableId 5, True )
                        ]
                    }
                  )
                ]
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


updateChartable : ChartableId -> (Chartable -> Chartable) -> UserData -> UserData
updateChartable id fn (UserData data) =
    UserData { data | chartables = data.chartables |> IdDict.update id fn }


addChartable : Chartable -> UserData -> ( Maybe ChartableId, UserData )
addChartable chartable (UserData data) =
    let
        ( newId, chartablesU ) =
            data.chartables |> IdDict.add chartable
    in
    ( newId, UserData { data | chartables = chartablesU } )


updateLineChart : LineChartId -> (LineChart -> LineChart) -> UserData -> UserData
updateLineChart id fn (UserData data) =
    UserData { data | lineCharts = data.lineCharts |> IdDict.update id fn }


decode : D.Decoder UserData
decode =
    let
        v0 =
            D.map (\ts -> UserData { trackables = ts, chartables = ChartableId.toDict [], lineCharts = LineChartId.toDict [] })
                (TrackableId.decodeDict Trackable.decode)

        v1 =
            D.map3 (\tbles cbles cts -> UserData { trackables = tbles, chartables = cbles, lineCharts = cts })
                (D.field "trackables" <| TrackableId.decodeDict Trackable.decode)
                (D.field "chartables" <| ChartableId.decodeDict Chartable.decode)
                (D.field "lineCharts" <| LineChartId.decodeDict LineChart.decode)
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
    E.object
        [ ( "version", E.int 1 )
        , ( "data"
          , E.object
                [ ( "trackables", TrackableId.encodeDict Trackable.encode data.trackables )
                , ( "chartables", ChartableId.encodeDict Chartable.encode data.chartables )
                , ( "lineCharts", LineChartId.encodeDict LineChart.encode data.lineCharts )
                ]
          )
        ]
