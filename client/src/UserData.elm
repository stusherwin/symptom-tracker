module UserData exposing (UserData, activeChartables, activeLineCharts, activeTrackables, addChartable, addLineChart, addTrackable, chartables, decode, deleteChartable, deleteLineChart, deleteTrackable, encode, getChartable, getLineChart, getTrackable, init, lineCharts, moveChartableDown, moveChartableUp, moveData, moveLineChartDown, moveLineChartUp, moveTrackableDown, moveTrackableUp, toggleChartableVisible, toggleTrackableVisible, trackables, updateChartable, updateLineChart, updateTrackable)

import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict
import Extra.List as List
import IdDict
import Json.Decode as D
import Json.Encode as E
import Svg.Icon exposing (IconType(..))
import Time exposing (Month(..))
import UserData.Chartable as C exposing (Chartable(..), ChartableDict)
import UserData.ChartableId as CId exposing (ChartableId(..))
import UserData.LineChart as LC exposing (LineChart, LineChartDict)
import UserData.LineChartId as LCId exposing (LineChartId(..))
import UserData.Trackable as T exposing (Responses(..), Trackable, TrackableDict)
import UserData.TrackableId as TId exposing (TrackableId(..))


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
    -- let
    --     tbles =
    --         TrackableId.toDict
    --             [ ( TrackableId 1
    --               , Trackable.build
    --                     { question = "How did you feel?"
    --                     , colour = Red
    --                     , responses =
    --                         TIcon (Array.fromList [ SolidTired, SolidFrownOpen, SolidMeh, SolidGrin, SolidLaughBeam ]) <|
    --                             Dict.fromList
    --                                 [ ( 737772, 3 ), ( 737773, 2 ), ( 737774, 3 ), ( 737789, 1 ), ( 737793, 1 ), ( 737811, 0 ), ( 737812, 1 ), ( 737813, 2 ), ( 737814, 4 ), ( 737815, 0 ), ( 737816, 2 ), ( 737817, 0 ), ( 737818, 3 ), ( 737819, 4 ), ( 737820, 2 ), ( 737821, 3 ), ( 737824, 0 ) ]
    --                     }
    --               )
    --             , ( TrackableId 2
    --               , Trackable.build
    --                     { question = "Did you have a bath?"
    --                     , colour = Green
    --                     , responses =
    --                         TYesNo <|
    --                             Dict.fromList
    --                                 [ ( 737789, True ), ( 737814, True ), ( 737820, False ), ( 737824, True ) ]
    --                     }
    --               )
    --             , ( TrackableId 3
    --               , Trackable.build
    --                     { question = "Did you smoke?"
    --                     , colour = Orange
    --                     , responses =
    --                         TYesNo <|
    --                             Dict.fromList
    --                                 [ ( 737772, True ), ( 737773, False ), ( 737789, False ), ( 737814, False ), ( 737820, True ), ( 737824, False ) ]
    --                     }
    --               )
    --             , ( TrackableId 4
    --               , Trackable.build
    --                     { question = "What was your energy level?"
    --                     , colour = Blue
    --                     , responses =
    --                         TScale 1
    --                             11
    --                         <|
    --                             Dict.fromList
    --                                 [ ( 737789, 3 ), ( 737814, 1 ), ( 737816, 5 ), ( 737817, 11 ), ( 737818, 0 ), ( 737820, 5 ), ( 737824, 3 ) ]
    --                     }
    --               )
    --             , ( TrackableId 5
    --               , Trackable.build
    --                     { question = "How many chocolate bars did you eat?"
    --                     , colour = Pink
    --                     , responses =
    --                         TInt <|
    --                             Dict.fromList
    --                                 [ ( 737789, 1 ), ( 737814, 1 ), ( 737820, 3 ), ( 737824, 2 ) ]
    --                     }
    --               )
    --             , ( TrackableId 6
    --               , Trackable.build
    --                     { question = "How many miles did you run?"
    --                     , colour = Purple
    --                     , responses =
    --                         TFloat <|
    --                             Dict.fromList
    --                                 [ ( 737789, 7 ), ( 737814, 2.5 ), ( 737815, 10 ), ( 737816, 0 ), ( 737817, 12 ), ( 737818, 1 ), ( 737819, 11 ), ( 737820, 2 ), ( 737821, 1 ), ( 737822, 10 ), ( 737824, 2.3 ) ]
    --                     }
    --               )
    --             , ( TrackableId 7
    --               , Trackable.build
    --                     { question = "Any other notes?"
    --                     , colour = Rose
    --                     , responses =
    --                         TText <|
    --                             Dict.fromList
    --                                 [ ( 737814, "fdsa" ), ( 737824, "xsdf" ) ]
    --                     }
    --               )
    --             ]
    --     cbles =
    --         ChartableId.toDict
    --             [ ( ChartableId 1
    --               , Chartable.build tbles
    --                     { name = "Mood"
    --                     , ownColour = Nothing
    --                     , isInverted = False
    --                     , sum =
    --                         [ ( TrackableId 1, 1.0 )
    --                         ]
    --                     }
    --               )
    --             , ( ChartableId 2
    --               , Chartable.build tbles
    --                     { name = "Bath"
    --                     , ownColour = Nothing
    --                     , isInverted = False
    --                     , sum =
    --                         [ ( TrackableId 2, 5.0 )
    --                         ]
    --                     }
    --               )
    --             , ( ChartableId 3
    --               , Chartable.build tbles
    --                     { name = "Bad things"
    --                     , ownColour = Just Colour.Orange
    --                     , isInverted = True
    --                     , sum =
    --                         [ ( TrackableId 3, 5.0 )
    --                         , ( TrackableId 5, 1.0 )
    --                         ]
    --                     }
    --               )
    --             , ( ChartableId 4
    --               , Chartable.build tbles
    --                     { name = "Energy"
    --                     , ownColour = Nothing
    --                     , isInverted = False
    --                     , sum =
    --                         [ ( TrackableId 4, 1.0 )
    --                         ]
    --                     }
    --               )
    --             , ( ChartableId 5
    --               , Chartable.build tbles
    --                     { name = "Running"
    --                     , ownColour = Nothing
    --                     , isInverted = False
    --                     , sum =
    --                         [ ( TrackableId 6, 1.0 )
    --                         ]
    --                     }
    --               )
    --             ]
    -- in
    UserData
        { trackables = TId.toDict []
        , chartables = CId.toDict []
        , lineCharts = LCId.toDict []

        -- LineChartId.toDict
        --     [ ( LineChartId 1
        --       , LineChart.build cbles
        --             tbles
        --             { name = "All Data"
        --             , fillLines = True
        --             , dataSets =
        --                 Array.fromList
        --                     [ ( LineChart.StateChartable { chartableId = ChartableId 1 }, True )
        --                     , ( LineChart.StateChartable { chartableId = ChartableId 2 }, False )
        --                     , ( LineChart.StateChartable { chartableId = ChartableId 3 }, True )
        --                     , ( LineChart.StateChartable { chartableId = ChartableId 4 }, True )
        --                     , ( LineChart.StateChartable { chartableId = ChartableId 5 }, True )
        --                     ]
        --             }
        --       )
        --     ]
        , activeTrackables = []

        -- [ ( TrackableId 1, True )
        -- , ( TrackableId 2, True )
        -- , ( TrackableId 3, True )
        -- ]
        , activeChartables = []

        -- [ ( ChartableId 1
        --   , True
        --   )
        -- , ( ChartableId 2
        --   , True
        --   )
        -- , ( ChartableId 3
        --   , True
        --   )
        -- , ( ChartableId 4
        --   , True
        --   )
        -- , ( ChartableId 5
        --   , True
        --   )
        -- ]
        , activeLineCharts = []

        -- [ LineChartId 1
        -- ]
        , errors = []
        }


updateTrackable : TrackableId -> (Trackable -> Result String Trackable) -> UserData -> UserData
updateTrackable id fn (UserData data) =
    case data.trackables |> IdDict.update id fn of
        Ok trackables_ ->
            UserData { data | trackables = trackables_ }

        Err err ->
            UserData { data | errors = err :: data.errors }


addTrackable : T.New -> UserData -> ( Maybe ( TrackableId, Trackable ), UserData )
addTrackable trackable (UserData data) =
    case data.trackables |> T.add trackable of
        Ok ( ( id, t ), trackables_ ) ->
            ( Just ( id, t )
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
    case data.trackables |> IdDict.delete id of
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
    UserData { data | activeTrackables = data.activeTrackables |> List.updateLookup id not }


moveTrackableUp : TrackableId -> UserData -> UserData
moveTrackableUp id (UserData data) =
    UserData { data | activeTrackables = data.activeTrackables |> List.moveHeadwardsBy Tuple.first id }


moveTrackableDown : TrackableId -> UserData -> UserData
moveTrackableDown id (UserData data) =
    UserData { data | activeTrackables = data.activeTrackables |> List.moveTailwardsBy Tuple.first id }


updateChartable : ChartableId -> (Chartable -> Result String Chartable) -> UserData -> UserData
updateChartable id fn (UserData data) =
    case data.chartables |> IdDict.update id fn of
        Ok chartables_ ->
            UserData { data | chartables = chartables_ }

        Err err ->
            UserData { data | errors = err :: data.errors }


addChartable : C.New -> UserData -> ( Maybe ( ChartableId, Chartable ), UserData )
addChartable chartable (UserData data) =
    case data.chartables |> C.add data.trackables chartable of
        Ok ( ( id, c ), chartables_ ) ->
            ( Just ( id, c )
            , UserData
                { data
                    | chartables = chartables_
                    , activeChartables = data.activeChartables ++ [ ( id, True ) ]
                }
            )

        Err err ->
            ( Nothing, UserData { data | errors = err :: data.errors } )


deleteChartable : ChartableId -> UserData -> UserData
deleteChartable id (UserData data) =
    case data.chartables |> IdDict.delete id of
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
    UserData { data | activeChartables = data.activeChartables |> List.updateLookup chartableId not }


moveChartableUp : ChartableId -> UserData -> UserData
moveChartableUp chartableId (UserData data) =
    UserData { data | activeChartables = data.activeChartables |> List.moveHeadwardsBy Tuple.first chartableId }


moveChartableDown : ChartableId -> UserData -> UserData
moveChartableDown chartableId (UserData data) =
    UserData { data | activeChartables = data.activeChartables |> List.moveTailwardsBy Tuple.first chartableId }


updateLineChart : LineChartId -> (LineChart -> Result String LineChart) -> UserData -> UserData
updateLineChart id fn (UserData data) =
    case data.lineCharts |> IdDict.update id fn of
        Ok lineCharts_ ->
            UserData { data | lineCharts = lineCharts_ }

        Err err ->
            UserData { data | errors = err :: data.errors }


addLineChart : LC.New -> UserData -> ( Maybe ( LineChartId, LineChart ), UserData )
addLineChart lineChartState (UserData data) =
    case data.lineCharts |> LC.add data.chartables data.trackables lineChartState of
        Ok ( ( id, chart ), lineCharts_ ) ->
            ( Just ( id, chart )
            , UserData
                { data
                    | lineCharts = lineCharts_
                    , activeLineCharts = data.activeLineCharts ++ [ id ]
                }
            )

        Err err ->
            ( Nothing, UserData { data | errors = err :: data.errors } )


moveLineChartUp : LineChartId -> UserData -> UserData
moveLineChartUp id (UserData data) =
    UserData { data | activeLineCharts = data.activeLineCharts |> List.moveHeadwards id }


moveLineChartDown : LineChartId -> UserData -> UserData
moveLineChartDown id (UserData data) =
    UserData { data | activeLineCharts = data.activeLineCharts |> List.moveTailwards id }


deleteLineChart : LineChartId -> UserData -> UserData
deleteLineChart id (UserData data) =
    case data.lineCharts |> IdDict.delete id of
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
                << List.filterMap (List.head << Dict.keys << T.onlyFloatData)
                << List.map Tuple.second
                << IdDict.toList
            <|
                data.trackables

        days =
            Date.diff Days maxDate today - 1
    in
    UserData
        { data
            | trackables =
                data.trackables
                    |> IdDict.map (always (T.moveData days))
        }


decode : D.Decoder UserData
decode =
    let
        v0 =
            D.map
                (\ts ->
                    UserData
                        { trackables = ts
                        , chartables = CId.toDict []
                        , lineCharts = LCId.toDict []
                        , activeTrackables = ts |> IdDict.map (\k _ -> ( k, True )) |> IdDict.values
                        , activeChartables = []
                        , activeLineCharts = []
                        , errors = []
                        }
                )
                (TId.decodeDict T.decode)

        v1 =
            D.map3
                (\tbles cbles cts ->
                    let
                        cbles_ =
                            C.buildDict tbles cbles

                        cts_ =
                            LC.buildDict cbles_ tbles cts
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
                (D.field "trackables" <| TId.decodeDict T.decode)
                (D.field "chartables" <| CId.decodeDict C.decode)
                (D.field "lineCharts" <| LCId.decodeDict LC.decode)

        v2 =
            D.map4
                (\tbles cbles cts atbles ->
                    let
                        cbles_ =
                            C.buildDict tbles cbles

                        cts_ =
                            LC.buildDict cbles_ tbles cts
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
                (D.field "trackables" <| TId.decodeDict T.decode)
                (D.field "chartables" <| CId.decodeDict C.decode)
                (D.field "lineCharts" <| LCId.decodeDict LC.decode)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TId.decode)
                            (D.field "visible" D.bool)
                )

        v3 =
            D.map5
                (\tbles cbles cts atbles acts ->
                    let
                        cbles_ =
                            C.buildDict tbles cbles

                        cts_ =
                            LC.buildDict cbles_ tbles cts
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
                (D.field "trackables" <| TId.decodeDict T.decode)
                (D.field "chartables" <| CId.decodeDict C.decode)
                (D.field "lineCharts" <| LCId.decodeDict LC.decode)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeCharts" <|
                    D.list LCId.decode
                )

        v4 =
            D.map6
                (\tbles cbles cts atbles acbles acts ->
                    let
                        cbles_ =
                            C.buildDict tbles cbles

                        cts_ =
                            LC.buildDict cbles_ tbles cts
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
                (D.field "trackables" <| TId.decodeDict T.decode)
                (D.field "chartables" <| CId.decodeDict C.decode)
                (D.field "lineCharts" <| LCId.decodeDict LC.decodeV5)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeChartables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" CId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeCharts" <|
                    D.list LCId.decode
                )

        v5 =
            D.map6
                (\tbles cbles cts atbles acbles acts ->
                    let
                        cbles_ =
                            C.buildDict tbles cbles

                        cts_ =
                            LC.buildDict cbles_ tbles cts
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
                (D.field "trackables" <| TId.decodeDict T.decode)
                (D.field "chartables" <| CId.decodeDict C.decode)
                (D.field "lineCharts" <| LCId.decodeDict LC.decodeV5)
                (D.field "activeTrackables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" TId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeChartables" <|
                    D.list <|
                        D.map2 Tuple.pair
                            (D.field "id" CId.decode)
                            (D.field "visible" D.bool)
                )
                (D.field "activeCharts" <|
                    D.list LCId.decode
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
                [ ( "trackables", TId.encodeDict T.encode data.trackables )
                , ( "chartables", CId.encodeDict C.encode data.chartables )
                , ( "lineCharts", LCId.encodeDict LC.encode data.lineCharts )
                , ( "activeTrackables"
                  , data.activeTrackables
                        |> E.list
                            (\( id, visible ) ->
                                E.object
                                    [ ( "id", TId.encode id )
                                    , ( "visible", E.bool visible )
                                    ]
                            )
                  )
                , ( "activeChartables"
                  , data.activeChartables
                        |> E.list
                            (\( id, visible ) ->
                                E.object
                                    [ ( "id", CId.encode id )
                                    , ( "visible", E.bool visible )
                                    ]
                            )
                  )
                , ( "activeCharts"
                  , data.activeLineCharts
                        |> E.list LCId.encode
                  )
                ]
          )
        ]
