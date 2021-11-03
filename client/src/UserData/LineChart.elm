module UserData.LineChart exposing (DataSet(..), LineChart, LineChartDict, New, StateDataSet(..), add, addChartable, addTrackable, buildDict, dataSets, decode, decodeV5, deleteData, encode, fillLines, moveDataDown, moveDataUp, name, replaceTrackable, replaceTrackableWithChartable, setFillLines, setName, setTrackableInverted, setTrackableMultiplier, toggleDataVisible)

import Array exposing (Array)
import Arrayx
import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import UserData.Chartable as C exposing (Chartable, ChartableDict)
import UserData.ChartableId as CId exposing (ChartableId)
import UserData.LineChartId exposing (LineChartId)
import UserData.Trackable as T exposing (Trackable, TrackableDict)
import UserData.TrackableId as TId exposing (TrackableId)


type LineChart
    = LineChart Data


type alias State =
    { name : String
    , fillLines : Bool
    , dataSets : Array ( StateDataSet, Bool )
    }


type alias New =
    { name : String
    , fillLines : Bool
    , dataSets : Array ( StateDataSet, Bool )
    }


type alias Data =
    { name : String
    , fillLines : Bool
    , dataSets : Array ( DataSet, Bool )
    }


type StateDataSet
    = StateChartable StateChartableData
    | StateTrackable StateTrackableData


type alias StateChartableData =
    { chartableId : ChartableId }


type alias StateTrackableData =
    { trackableId : TrackableId
    , multiplier : Float
    , isInverted : Bool
    }


type DataSet
    = Chartable
        { chartableId : ChartableId
        , chartable : Chartable
        }
    | Trackable
        { trackableId : TrackableId
        , trackable : Trackable
        , multiplier : Float
        , isInverted : Bool
        }


type alias LineChartDict =
    IdDict LineChartId LineChart


name : LineChart -> String
name (LineChart c) =
    c.name


fillLines : LineChart -> Bool
fillLines (LineChart c) =
    c.fillLines


dataSets : LineChart -> Array ( DataSet, Bool )
dataSets (LineChart c) =
    c.dataSets


add : ChartableDict -> TrackableDict -> New -> LineChartDict -> Result String ( ( LineChartId, LineChart ), LineChartDict )
add chartables trackables c dict =
    let
        chart =
            build chartables trackables c
    in
    dict |> IdDict.tryAdd chart |> Result.map (\( id, dict_ ) -> ( ( id, chart ), dict_ ))


setName : String -> LineChart -> LineChart
setName n (LineChart c) =
    LineChart { c | name = n }


setFillLines : Bool -> LineChart -> LineChart
setFillLines fl (LineChart c) =
    LineChart { c | fillLines = fl }


addChartable : ChartableId -> Chartable -> LineChart -> LineChart
addChartable id chartable (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Array.push ( Chartable { chartableId = id, chartable = chartable }, True ) }


addTrackable : TrackableId -> Trackable -> LineChart -> LineChart
addTrackable id trackable (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Array.push ( Trackable { trackableId = id, trackable = trackable, multiplier = 1, isInverted = False }, True ) }


replaceTrackableWithChartable : Int -> ChartableId -> Chartable -> LineChart -> LineChart
replaceTrackableWithChartable i id chartable (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Array.set i ( Chartable { chartableId = id, chartable = chartable }, True ) }


replaceTrackable : Int -> TrackableId -> Trackable -> Float -> Bool -> LineChart -> LineChart
replaceTrackable i id trackable multiplier isInverted (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Array.set i ( Trackable { trackableId = id, trackable = trackable, multiplier = multiplier, isInverted = isInverted }, True ) }


setTrackableInverted : Int -> Bool -> LineChart -> LineChart
setTrackableInverted i isInverted (LineChart c) =
    LineChart
        { c
            | dataSets =
                c.dataSets
                    |> Arrayx.update i
                        (\d ->
                            case d of
                                ( Trackable t, visible ) ->
                                    ( Trackable { t | isInverted = isInverted }, visible )

                                _ ->
                                    d
                        )
        }


setTrackableMultiplier : Int -> Float -> LineChart -> LineChart
setTrackableMultiplier i multiplier (LineChart c) =
    LineChart
        { c
            | dataSets =
                c.dataSets
                    |> Arrayx.update i
                        (\d ->
                            case d of
                                ( Trackable t, visible ) ->
                                    ( Trackable { t | multiplier = multiplier }, visible )

                                _ ->
                                    d
                        )
        }


deleteData : Int -> LineChart -> LineChart
deleteData i (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Arrayx.delete i }


toggleDataVisible : Int -> LineChart -> LineChart
toggleDataVisible i (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Arrayx.update i (Tuple.mapSecond not) }


moveDataUp : Int -> LineChart -> LineChart
moveDataUp i (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Arrayx.swap i (i - 1) }


moveDataDown : Int -> LineChart -> LineChart
moveDataDown i (LineChart c) =
    LineChart
        { c | dataSets = c.dataSets |> Arrayx.swap i (i + 1) }


buildDict : ChartableDict -> TrackableDict -> IdDict LineChartId State -> LineChartDict
buildDict chartables trackables states =
    IdDict.map (\_ s -> build chartables trackables s) states


build : ChartableDict -> TrackableDict -> State -> LineChart
build chartables trackables s =
    LineChart
        { name = s.name
        , fillLines = s.fillLines
        , dataSets =
            s.dataSets
                |> Array.toList
                |> List.filterMap
                    (\( d, visible ) ->
                        case d of
                            StateChartable { chartableId } ->
                                chartables
                                    |> IdDict.get chartableId
                                    |> Maybe.map (\chartable -> ( Chartable { chartableId = chartableId, chartable = chartable }, visible ))

                            StateTrackable { trackableId, multiplier, isInverted } ->
                                trackables
                                    |> IdDict.get trackableId
                                    |> Maybe.map
                                        (\trackable ->
                                            ( Trackable
                                                { trackableId = trackableId
                                                , trackable = trackable
                                                , multiplier = multiplier
                                                , isInverted = isInverted
                                                }
                                            , visible
                                            )
                                        )
                    )
                |> Array.fromList
        }


state : LineChart -> State
state (LineChart c) =
    { name = c.name
    , fillLines = c.fillLines
    , dataSets =
        c.dataSets
            |> Array.toList
            |> List.map
                (Tuple.mapFirst
                    (\d ->
                        case d of
                            Chartable { chartableId } ->
                                StateChartable { chartableId = chartableId }

                            Trackable { trackableId, multiplier, isInverted } ->
                                StateTrackable { trackableId = trackableId, multiplier = multiplier, isInverted = isInverted }
                    )
                )
            |> Array.fromList
    }


decode : D.Decoder State
decode =
    D.map3
        (\n fl cbles ->
            { name = n
            , dataSets = cbles |> List.map (Tuple.mapFirst (StateChartable << StateChartableData)) |> Array.fromList
            , fillLines = fl
            }
        )
        (D.field "name" D.string)
        (D.field "fillLines" D.bool)
        (D.field "data" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "id" CId.decode)
                    (D.field "visible" D.bool)
        )


decodeV5 : D.Decoder State
decodeV5 =
    D.map3
        (\n fl ds ->
            { name = n
            , dataSets = ds |> Array.fromList
            , fillLines = fl
            }
        )
        (D.field "name" D.string)
        (D.field "fillLines" D.bool)
        (D.field "data" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "data" decodeData)
                    (D.field "visible" D.bool)
        )


decodeData : D.Decoder StateDataSet
decodeData =
    D.oneOf
        [ D.map (StateChartable << StateChartableData) CId.decode
        , D.map StateTrackable <|
            D.map3
                StateTrackableData
                (D.field "id" TId.decode)
                (D.field "multiplier" D.float)
                (D.field "inverted" D.bool)
        ]


encodeData : StateDataSet -> E.Value
encodeData d =
    case d of
        StateChartable { chartableId } ->
            CId.encode chartableId

        StateTrackable { trackableId, multiplier, isInverted } ->
            E.object
                [ ( "id", TId.encode trackableId )
                , ( "multiplier", E.float multiplier )
                , ( "inverted", E.bool isInverted )
                ]


encode : LineChart -> E.Value
encode chart =
    let
        s =
            state chart
    in
    E.object
        [ ( "name", E.string s.name )
        , ( "fillLines", E.bool s.fillLines )
        , ( "data"
          , s.dataSets
                |> Array.toList
                |> E.list
                    (\( ds, visible ) ->
                        E.object
                            [ ( "data", encodeData ds )
                            , ( "visible", E.bool visible )
                            ]
                    )
          )
        ]
