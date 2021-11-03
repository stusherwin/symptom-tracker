module UserData exposing (UserData, activeChartables, activeLineCharts, activeTrackables, addChartable, addLineChart, addTrackable, chartables, decode, deleteChartable, deleteLineChart, deleteTrackable, encode, getChartable, getLineChart, getTrackable, init, lineCharts, moveChartableDown, moveChartableUp, moveData, moveLineChartDown, moveLineChartUp, moveTrackableDown, moveTrackableUp, toggleChartableVisible, toggleTrackableVisible, trackables, updateChartable, updateLineChart, updateTrackable)

import Array
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Dictx
import IdDict
import Json.Decode as D
import Json.Encode as E
import Listx
import Svg.Icon exposing (IconType(..))
import Time exposing (Month(..))
import UserData.Chartable as Chartable exposing (Chartable(..), ChartableDict)
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
        , activeTrackables : List ( TrackableId, Bool )
        , activeChartables : List ( ChartableId, Bool )
        , activeLineCharts : List LineChartId
        , errors : List String
        }


trackables : UserData -> TrackableDict
trackables (UserData data) =
    data.trackables


activeTrackables : UserData -> List ( TrackableId, ( Trackable, Bool ) )
activeTrackables (UserData data) =
    data.activeTrackables
        |> List.filterMap (\( id, visible ) -> data.trackables |> IdDict.get id |> Maybe.map (\t -> ( id, ( t, visible ) )))


getTrackable : TrackableId -> UserData -> Maybe Trackable
getTrackable id (UserData data) =
    IdDict.get id data.trackables


chartables : UserData -> ChartableDict
chartables (UserData data) =
    data.chartables


activeChartables : UserData -> List ( ChartableId, ( Chartable, Bool ) )
activeChartables (UserData data) =
    data.activeChartables
        |> List.filterMap (\( id, visible ) -> data.chartables |> IdDict.get id |> Maybe.map (\t -> ( id, ( t, visible ) )))


getChartable : ChartableId -> UserData -> Maybe Chartable
getChartable id (UserData data) =
    IdDict.get id data.chartables


lineCharts : UserData -> LineChartDict
lineCharts (UserData data) =
    data.lineCharts


activeLineCharts : UserData -> List ( LineChartId, LineChart )
activeLineCharts (UserData data) =
    data.activeLineCharts
        |> List.filterMap (\id -> data.lineCharts |> IdDict.get id |> Maybe.map (\t -> ( id, t )))


getLineChart : LineChartId -> UserData -> Maybe LineChart
getLineChart id (UserData data) =
    IdDict.get id data.lineCharts


init : UserData
init =
    let
        tbles =
            TrackableId.toDict
                [ ( TrackableId 1
                  , { question = "How did you feel?"
                    , colour = Red
                    , data =
                        TIcon (Array.fromList [ SolidTired, SolidFrownOpen, SolidMeh, SolidGrin, SolidLaughBeam ]) <|
                            Dict.fromList
                                [ ( 737772, 3 ), ( 737773, 2 ), ( 737774, 3 ), ( 737789, 1 ), ( 737793, 1 ), ( 737811, 0 ), ( 737812, 1 ), ( 737813, 2 ), ( 737814, 4 ), ( 737815, 0 ), ( 737816, 2 ), ( 737817, 0 ), ( 737818, 3 ), ( 737819, 4 ), ( 737820, 2 ), ( 737821, 3 ), ( 737824, 0 ) ]
                    }
                  )
                , ( TrackableId 2
                  , { question = "Did you have a bath?"
                    , colour = Green
                    , data =
                        TYesNo <|
                            Dict.fromList
                                [ ( 737789, True ), ( 737814, True ), ( 737820, False ), ( 737824, True ) ]
                    }
                  )
                , ( TrackableId 3
                  , { question = "Did you smoke?"
                    , colour = Orange
                    , data =
                        TYesNo <|
                            Dict.fromList
                                [ ( 737772, True ), ( 737773, False ), ( 737789, False ), ( 737814, False ), ( 737820, True ), ( 737824, False ) ]
                    }
                  )
                , ( TrackableId 4
                  , { question = "What was your energy level?"
                    , colour = Blue
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
                    , data =
                        TInt <|
                            Dict.fromList
                                [ ( 737789, 1 ), ( 737814, 1 ), ( 737820, 3 ), ( 737824, 2 ) ]
                    }
                  )
                , ( TrackableId 6
                  , { question = "How many miles did you run?"
                    , colour = Purple
                    , data =
                        TFloat <|
                            Dict.fromList
                                [ ( 737789, 7 ), ( 737814, 2.5 ), ( 737815, 10 ), ( 737816, 0 ), ( 737817, 12 ), ( 737818, 1 ), ( 737819, 11 ), ( 737820, 2 ), ( 737821, 1 ), ( 737822, 10 ), ( 737824, 2.3 ) ]
                    }
                  )
                , ( TrackableId 7
                  , { question = "Any other notes?"
                    , colour = Rose
                    , data =
                        TText <|
                            Dict.fromList
                                [ ( 737814, "fdsa" ), ( 737824, "xsdf" ) ]
                    }
                  )
                ]

        cbles =
            ChartableId.toDict
                [ ( ChartableId 1
                  , Chartable.build tbles
                        { name = "Mood"
                        , colour = Nothing
                        , inverted = False
                        , sum =
                            [ ( TrackableId 1, 1.0 )
                            ]
                        }
                  )
                , ( ChartableId 2
                  , Chartable.build tbles
                        { name = "Bath"
                        , colour = Nothing
                        , inverted = False
                        , sum =
                            [ ( TrackableId 2, 5.0 )
                            ]
                        }
                  )
                , ( ChartableId 3
                  , Chartable.build tbles
                        { name = "Bad things"
                        , colour = Just Colour.Orange
                        , inverted = True
                        , sum =
                            [ ( TrackableId 3, 5.0 )
                            , ( TrackableId 5, 1.0 )
                            ]
                        }
                  )
                , ( ChartableId 4
                  , Chartable.build tbles
                        { name = "Energy"
                        , colour = Nothing
                        , inverted = False
                        , sum =
                            [ ( TrackableId 4, 1.0 )
                            ]
                        }
                  )
                , ( ChartableId 5
                  , Chartable.build tbles
                        { name = "Running"
                        , colour = Nothing
                        , inverted = False
                        , sum =
                            [ ( TrackableId 6, 1.0 )
                            ]
                        }
                  )
                ]
    in
    UserData
        { trackables = tbles
        , chartables = cbles
        , lineCharts =
            LineChartId.toDict
                [ ( LineChartId 1
                  , LineChart.build cbles
                        tbles
                        { name = "All Data"
                        , fillLines = True
                        , data =
                            Array.fromList
                                [ ( LineChart.StateChartable { chartableId = ChartableId 1 }, True )
                                , ( LineChart.StateChartable { chartableId = ChartableId 2 }, False )
                                , ( LineChart.StateChartable { chartableId = ChartableId 3 }, True )
                                , ( LineChart.StateChartable { chartableId = ChartableId 4 }, True )
                                , ( LineChart.StateChartable { chartableId = ChartableId 5 }, True )
                                ]
                        }
                  )
                ]
        , activeTrackables =
            [ ( TrackableId 1, True )
            , ( TrackableId 2, True )
            , ( TrackableId 3, True )
            ]
        , activeChartables =
            [ ( ChartableId 1
              , True
              )
            , ( ChartableId 2
              , True
              )
            , ( ChartableId 3
              , True
              )
            , ( ChartableId 4
              , True
              )
            , ( ChartableId 5
              , True
              )
            ]
        , activeLineCharts =
            [ LineChartId 1
            ]
        , errors = []
        }


updateTrackable : TrackableId -> (Trackable -> Result String Trackable) -> UserData -> UserData
updateTrackable id fn (UserData data) =
    case data.trackables |> IdDict.tryUpdate id fn of
        Ok trackables_ ->
            UserData { data | trackables = trackables_ }

        Err err ->
            UserData { data | errors = err :: data.errors }


addTrackable : Trackable -> UserData -> ( Maybe TrackableId, UserData )
addTrackable trackable (UserData data) =
    case data.trackables |> IdDict.tryAdd trackable of
        Ok ( id, trackables_ ) ->
            ( Just id
            , UserData
                { data
                    | trackables = trackables_
                    , activeTrackables = data.activeTrackables ++ [ ( id, True ) ]
                }
            )

        Err err ->
            ( Nothing, UserData { data | errors = err :: data.errors } )


deleteTrackable : TrackableId -> UserData -> UserData
deleteTrackable id (UserData data) =
    case data.trackables |> IdDict.tryDelete id of
        Ok trackables_ ->
            UserData
                { data
                    | trackables = trackables_
                    , activeTrackables = data.activeTrackables |> List.filter (\( tId, _ ) -> tId /= id)
                }

        Err err ->
            UserData { data | errors = err :: data.errors }


toggleTrackableVisible : TrackableId -> UserData -> UserData
toggleTrackableVisible id (UserData data) =
    UserData { data | activeTrackables = data.activeTrackables |> Listx.updateLookup id not }


moveTrackableUp : TrackableId -> UserData -> UserData
moveTrackableUp id (UserData data) =
    UserData { data | activeTrackables = data.activeTrackables |> Listx.moveHeadwardsBy Tuple.first id }


moveTrackableDown : TrackableId -> UserData -> UserData
moveTrackableDown id (UserData data) =
    UserData { data | activeTrackables = data.activeTrackables |> Listx.moveTailwardsBy Tuple.first id }


updateChartable : ChartableId -> (Chartable -> Chartable) -> UserData -> UserData
updateChartable id fn (UserData data) =
    UserData { data | chartables = data.chartables |> IdDict.update id fn }


addChartable : Chartable.State -> UserData -> ( Maybe ( ChartableId, Chartable ), UserData )
addChartable chartableState (UserData data) =
    let
        chartable =
            Chartable.build data.trackables chartableState

        ( newId, chartablesU ) =
            data.chartables |> IdDict.add chartable
    in
    case newId of
        Just id ->
            ( Just ( id, chartable )
            , UserData
                { data
                    | chartables = chartablesU
                    , activeChartables = data.activeChartables ++ [ ( id, True ) ]
                }
            )

        _ ->
            ( Nothing, UserData data )


deleteChartable : ChartableId -> UserData -> UserData
deleteChartable id (UserData data) =
    case data.chartables |> IdDict.tryDelete id of
        Ok chartables_ ->
            UserData
                { data
                    | chartables = chartables_
                    , activeChartables = data.activeChartables |> List.filter (\( cId, _ ) -> cId /= id)
                }

        Err err ->
            UserData { data | errors = err :: data.errors }


toggleChartableVisible : ChartableId -> UserData -> UserData
toggleChartableVisible chartableId (UserData data) =
    UserData { data | activeChartables = data.activeChartables |> Listx.updateLookup chartableId not }


moveChartableUp : ChartableId -> UserData -> UserData
moveChartableUp chartableId (UserData data) =
    UserData { data | activeChartables = data.activeChartables |> Listx.moveHeadwardsBy Tuple.first chartableId }


moveChartableDown : ChartableId -> UserData -> UserData
moveChartableDown chartableId (UserData data) =
    UserData { data | activeChartables = data.activeChartables |> Listx.moveTailwardsBy Tuple.first chartableId }


updateLineChart : LineChartId -> (LineChart -> LineChart) -> UserData -> UserData
updateLineChart id fn (UserData data) =
    UserData { data | lineCharts = data.lineCharts |> IdDict.update id fn }


addLineChart : LineChart.State -> UserData -> ( Maybe ( LineChartId, LineChart ), UserData )
addLineChart lineChartState (UserData data) =
    let
        lineChart =
            LineChart.build data.chartables data.trackables lineChartState

        ( newId, lineChartsU ) =
            data.lineCharts |> IdDict.add lineChart
    in
    case newId of
        Just id ->
            ( Just ( id, lineChart )
            , UserData
                { data
                    | lineCharts = lineChartsU
                    , activeLineCharts = data.activeLineCharts ++ [ id ]
                }
            )

        _ ->
            ( Nothing, UserData data )


moveLineChartUp : LineChartId -> UserData -> UserData
moveLineChartUp id (UserData data) =
    UserData { data | activeLineCharts = data.activeLineCharts |> Listx.moveHeadwards id }


moveLineChartDown : LineChartId -> UserData -> UserData
moveLineChartDown id (UserData data) =
    UserData { data | activeLineCharts = data.activeLineCharts |> Listx.moveTailwards id }


deleteLineChart : LineChartId -> UserData -> UserData
deleteLineChart id (UserData data) =
    case data.lineCharts |> IdDict.tryDelete id of
        Ok lineCharts_ ->
            UserData
                { data
                    | lineCharts = lineCharts_
                    , activeLineCharts = data.activeLineCharts |> List.filter (\cId -> cId /= id)
                }

        Err err ->
            UserData { data | errors = err :: data.errors }


moveData : Date -> UserData -> UserData
moveData today (UserData data) =
    let
        maxDate =
            Date.fromRataDie
                << Maybe.withDefault (Date.toRataDie today)
                << List.maximum
                << List.filterMap (List.head << Dict.keys << Trackable.onlyFloatData)
                << List.map Tuple.second
                << IdDict.toList
            <|
                data.trackables

        days =
            4

        --Date.diff Days maxDate today - 1
    in
    UserData
        { data
            | trackables =
                data.trackables
                    |> IdDict.map
                        (\_ t ->
                            { t
                                | data =
                                    case t.data of
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
                        )
        }


decode : D.Decoder UserData
decode =
    let
        v0 =
            D.map
                (\ts ->
                    UserData
                        { trackables = ts
                        , chartables = ChartableId.toDict []
                        , lineCharts = LineChartId.toDict []
                        , activeTrackables = ts |> IdDict.map (\k _ -> ( k, True )) |> IdDict.values
                        , activeChartables = []
                        , activeLineCharts = []
                        , errors = []
                        }
                )
                (TrackableId.decodeDict Trackable.decode)

        v1 =
            D.map3
                (\tbles cbles cts ->
                    let
                        cbles_ =
                            IdDict.map (\_ s -> Chartable.build tbles s) cbles

                        cts_ =
                            IdDict.map (\_ s -> LineChart.build cbles_ tbles s) cts
                    in
                    UserData
                        { trackables = tbles
                        , chartables = cbles_
                        , lineCharts = cts_
                        , activeTrackables = tbles |> IdDict.map (\k _ -> ( k, True )) |> IdDict.values
                        , activeChartables = cbles_ |> IdDict.map (\k _ -> ( k, True )) |> IdDict.values
                        , activeLineCharts = cts |> IdDict.keys
                        , errors = []
                        }
                )
                (D.field "trackables" <| TrackableId.decodeDict Trackable.decode)
                (D.field "chartables" <| ChartableId.decodeDict Chartable.decode)
                (D.field "lineCharts" <| LineChartId.decodeDict LineChart.decode)

        v2 =
            D.map4
                (\tbles cbles cts atbles ->
                    let
                        cbles_ =
                            IdDict.map (\_ s -> Chartable.build tbles s) cbles

                        cts_ =
                            IdDict.map (\_ s -> LineChart.build cbles_ tbles s) cts
                    in
                    UserData
                        { trackables = tbles
                        , chartables = cbles_
                        , lineCharts = cts_
                        , activeTrackables = atbles
                        , activeChartables = cbles_ |> IdDict.map (\k _ -> ( k, True )) |> IdDict.values
                        , activeLineCharts = cts |> IdDict.keys
                        , errors = []
                        }
                )
                (D.field "trackables" <| TrackableId.decodeDict Trackable.decode)
                (D.field "chartables" <| ChartableId.decodeDict Chartable.decode)
                (D.field "lineCharts" <| LineChartId.decodeDict LineChart.decode)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TrackableId.decode)
                            (D.field "visible" D.bool)
                )

        v3 =
            D.map5
                (\tbles cbles cts atbles acts ->
                    let
                        cbles_ =
                            IdDict.map (\_ s -> Chartable.build tbles s) cbles

                        cts_ =
                            IdDict.map (\_ s -> LineChart.build cbles_ tbles s) cts
                    in
                    UserData
                        { trackables = tbles
                        , chartables = cbles_
                        , lineCharts = cts_
                        , activeTrackables = atbles
                        , activeChartables = cbles_ |> IdDict.map (\k _ -> ( k, True )) |> IdDict.values
                        , activeLineCharts = acts
                        , errors = []
                        }
                )
                (D.field "trackables" <| TrackableId.decodeDict Trackable.decode)
                (D.field "chartables" <| ChartableId.decodeDict Chartable.decode)
                (D.field "lineCharts" <| LineChartId.decodeDict LineChart.decode)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TrackableId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeCharts" <|
                    D.list LineChartId.decode
                )

        v4 =
            D.map6
                (\tbles cbles cts atbles acbles acts ->
                    let
                        cbles_ =
                            IdDict.map (\_ s -> Chartable.build tbles s) cbles

                        cts_ =
                            IdDict.map (\_ s -> LineChart.build cbles_ tbles s) cts
                    in
                    UserData
                        { trackables = tbles
                        , chartables = cbles_
                        , lineCharts = cts_
                        , activeTrackables = atbles
                        , activeChartables = acbles
                        , activeLineCharts = acts
                        , errors = []
                        }
                )
                (D.field "trackables" <| TrackableId.decodeDict Trackable.decode)
                (D.field "chartables" <| ChartableId.decodeDict Chartable.decode)
                (D.field "lineCharts" <| LineChartId.decodeDict LineChart.decodeV5)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TrackableId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeChartables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" ChartableId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeCharts" <|
                    D.list LineChartId.decode
                )

        v5 =
            D.map6
                (\tbles cbles cts atbles acbles acts ->
                    let
                        cbles_ =
                            IdDict.map (\_ s -> Chartable.build tbles s) cbles

                        cts_ =
                            IdDict.map (\_ s -> LineChart.build cbles_ tbles s) cts
                    in
                    UserData
                        { trackables = tbles
                        , chartables = cbles_
                        , lineCharts = cts_
                        , activeTrackables = atbles
                        , activeChartables = acbles
                        , activeLineCharts = acts
                        , errors = []
                        }
                )
                (D.field "trackables" <| TrackableId.decodeDict Trackable.decode)
                (D.field "chartables" <| ChartableId.decodeDict Chartable.decode)
                (D.field "lineCharts" <| LineChartId.decodeDict LineChart.decodeV5)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TrackableId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeChartables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" ChartableId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeCharts" <|
                    D.list LineChartId.decode
                )
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

                        2 ->
                            D.field "data" v2

                        3 ->
                            D.field "data" v3

                        4 ->
                            D.field "data" v4

                        5 ->
                            D.field "data" v5

                        v ->
                            D.fail <| "Unknown version " ++ String.fromInt v
                )
        ]


encode : UserData -> E.Value
encode (UserData data) =
    E.object
        [ ( "version", E.int 4 )
        , ( "data"
          , E.object
                [ ( "trackables", TrackableId.encodeDict Trackable.encode data.trackables )
                , ( "chartables", ChartableId.encodeDict Chartable.encode data.chartables )
                , ( "lineCharts", LineChartId.encodeDict LineChart.encode data.lineCharts )
                , ( "activeTrackables"
                  , data.activeTrackables
                        |> E.list
                            (\( id, visible ) ->
                                E.object
                                    [ ( "id", TrackableId.encode id )
                                    , ( "visible", E.bool visible )
                                    ]
                            )
                  )
                , ( "activeChartables"
                  , data.activeChartables
                        |> E.list
                            (\( id, visible ) ->
                                E.object
                                    [ ( "id", ChartableId.encode id )
                                    , ( "visible", E.bool visible )
                                    ]
                            )
                  )
                , ( "activeCharts"
                  , data.activeLineCharts
                        |> E.list LineChartId.encode
                  )
                ]
          )
        ]
