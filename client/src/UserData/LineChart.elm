module UserData.LineChart exposing (LineChart(..), LineChartDict, Presentation, PresentationDataSet(..), State, StateChartableData, StateDataSet(..), StateTrackableData, addChartable, addTrackable, build, decode, decodeV5, deleteData, encode, moveDataDown, moveDataUp, replaceTrackable, replaceTrackableWithChartable, setFillLines, setName, setTrackableInverted, setTrackableMultiplier, toggleDataVisible)

import Array exposing (Array)
import Arrayx
import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import UserData.Chartable as Chartable exposing (Chartable, ChartableDict)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChartId exposing (LineChartId)
import UserData.Trackable as Trackable exposing (Trackable, TrackableDict)
import UserData.TrackableId as TrackableId exposing (TrackableId)


type LineChart
    = LineChart State Presentation


type alias State =
    { name : String
    , fillLines : Bool
    , data : Array ( StateDataSet, Bool )
    }


type alias Presentation =
    { data : Array ( PresentationDataSet, Bool ) }


type StateDataSet
    = StateChartable StateChartableData
    | StateTrackable StateTrackableData


type alias StateChartableData =
    { chartableId : ChartableId }


type alias StateTrackableData =
    { trackableId : TrackableId
    , multiplier : Float
    , inverted : Bool
    }


type PresentationDataSet
    = PresentationChartable { chartableId : ChartableId, chartable : Chartable }
    | PresentationTrackable
        { trackableId : TrackableId
        , trackable : Trackable
        , multiplier : Float
        , inverted : Bool
        }


type alias LineChartDict =
    IdDict LineChartId LineChart


setName : String -> LineChart -> LineChart
setName name (LineChart s p) =
    LineChart { s | name = name } p


setFillLines : Bool -> LineChart -> LineChart
setFillLines fl (LineChart s p) =
    LineChart { s | fillLines = fl } p


addChartable : ChartableId -> Chartable -> LineChart -> LineChart
addChartable id chartable (LineChart s p) =
    LineChart
        { s | data = s.data |> Array.push ( StateChartable { chartableId = id }, True ) }
        { p | data = p.data |> Array.push ( PresentationChartable { chartableId = id, chartable = chartable }, True ) }


addTrackable : TrackableId -> Trackable -> LineChart -> LineChart
addTrackable id trackable (LineChart s p) =
    LineChart
        { s | data = s.data |> Array.push ( StateTrackable { trackableId = id, multiplier = 1, inverted = False }, True ) }
        { p | data = p.data |> Array.push ( PresentationTrackable { trackableId = id, trackable = trackable, multiplier = 1, inverted = False }, True ) }


replaceTrackableWithChartable : Int -> ChartableId -> Chartable -> LineChart -> LineChart
replaceTrackableWithChartable i id chartable (LineChart s p) =
    LineChart
        { s | data = s.data |> Array.set i ( StateChartable { chartableId = id }, True ) }
        { p | data = p.data |> Array.set i ( PresentationChartable { chartableId = id, chartable = chartable }, True ) }


replaceTrackable : Int -> TrackableId -> Trackable -> Float -> Bool -> LineChart -> LineChart
replaceTrackable i id trackable multiplier inverted (LineChart s p) =
    LineChart
        { s | data = s.data |> Array.set i ( StateTrackable { trackableId = id, multiplier = multiplier, inverted = inverted }, True ) }
        { p | data = p.data |> Array.set i ( PresentationTrackable { trackableId = id, trackable = trackable, multiplier = multiplier, inverted = inverted }, True ) }


setTrackableInverted : Int -> Bool -> LineChart -> LineChart
setTrackableInverted i inverted (LineChart s p) =
    LineChart
        { s
            | data =
                s.data
                    |> Arrayx.update i
                        (\d ->
                            case d of
                                ( StateTrackable t, visible ) ->
                                    ( StateTrackable { t | inverted = inverted }, visible )

                                _ ->
                                    d
                        )
        }
        { p
            | data =
                p.data
                    |> Arrayx.update i
                        (\d ->
                            case d of
                                ( PresentationTrackable t, visible ) ->
                                    ( PresentationTrackable { t | inverted = inverted }, visible )

                                _ ->
                                    d
                        )
        }


setTrackableMultiplier : Int -> Float -> LineChart -> LineChart
setTrackableMultiplier i multiplier (LineChart s p) =
    LineChart
        { s
            | data =
                s.data
                    |> Arrayx.update i
                        (\d ->
                            case d of
                                ( StateTrackable t, visible ) ->
                                    ( StateTrackable { t | multiplier = multiplier }, visible )

                                _ ->
                                    d
                        )
        }
        { p
            | data =
                p.data
                    |> Arrayx.update i
                        (\d ->
                            case d of
                                ( PresentationTrackable t, visible ) ->
                                    ( PresentationTrackable { t | multiplier = multiplier }, visible )

                                _ ->
                                    d
                        )
        }


deleteData : Int -> LineChart -> LineChart
deleteData i (LineChart s p) =
    LineChart
        { s | data = s.data |> Arrayx.delete i }
        { p | data = p.data |> Arrayx.delete i }


toggleDataVisible : Int -> LineChart -> LineChart
toggleDataVisible i (LineChart s p) =
    LineChart
        { s | data = s.data |> Arrayx.update i (Tuple.mapSecond not) }
        { p | data = p.data |> Arrayx.update i (Tuple.mapSecond not) }


moveDataUp : Int -> LineChart -> LineChart
moveDataUp i (LineChart s p) =
    LineChart
        { s | data = s.data |> Arrayx.swap i (i - 1) }
        { p | data = p.data |> Arrayx.swap i (i - 1) }


moveDataDown : Int -> LineChart -> LineChart
moveDataDown i (LineChart s p) =
    LineChart
        { s | data = s.data |> Arrayx.swap i (i + 1) }
        { p | data = p.data |> Arrayx.swap i (i + 1) }


build : ChartableDict -> TrackableDict -> State -> LineChart
build chartables trackables s =
    LineChart s
        { data =
            s.data
                |> Array.toList
                |> List.filterMap
                    (\( d, visible ) ->
                        case d of
                            StateChartable { chartableId } ->
                                chartables
                                    |> IdDict.get chartableId
                                    |> Maybe.map (\chartable -> ( PresentationChartable { chartableId = chartableId, chartable = chartable }, visible ))

                            StateTrackable { trackableId, multiplier, inverted } ->
                                trackables
                                    |> IdDict.get trackableId
                                    |> Maybe.map
                                        (\trackable ->
                                            ( PresentationTrackable
                                                { trackableId = trackableId
                                                , trackable = trackable
                                                , multiplier = multiplier
                                                , inverted = inverted
                                                }
                                            , visible
                                            )
                                        )
                    )
                |> Array.fromList
        }


decode : D.Decoder State
decode =
    D.map3
        (\name fillLines cbles ->
            { name = name
            , data = cbles |> List.map (Tuple.mapFirst (StateChartable << StateChartableData)) |> Array.fromList
            , fillLines = fillLines
            }
        )
        (D.field "name" D.string)
        (D.field "fillLines" D.bool)
        (D.field "data" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "id" ChartableId.decode)
                    (D.field "visible" D.bool)
        )


decodeV5 : D.Decoder State
decodeV5 =
    D.map3
        (\name fillLines data ->
            { name = name
            , data = data |> Array.fromList
            , fillLines = fillLines
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
        [ D.map (StateChartable << StateChartableData) ChartableId.decode
        , D.map StateTrackable <|
            D.map3
                StateTrackableData
                (D.field "id" TrackableId.decode)
                (D.field "multiplier" D.float)
                (D.field "inverted" D.bool)
        ]


encodeData : StateDataSet -> E.Value
encodeData d =
    case d of
        StateChartable { chartableId } ->
            ChartableId.encode chartableId

        StateTrackable { trackableId, multiplier, inverted } ->
            E.object
                [ ( "id", TrackableId.encode trackableId )
                , ( "multiplier", E.float multiplier )
                , ( "inverted", E.bool inverted )
                ]


encode : LineChart -> E.Value
encode (LineChart s _) =
    E.object
        [ ( "name", E.string s.name )
        , ( "fillLines", E.bool s.fillLines )
        , ( "data"
          , s.data
                |> Array.toList
                |> E.list
                    (\( data, visible ) ->
                        E.object
                            [ ( "data", encodeData data )
                            , ( "visible", E.bool visible )
                            ]
                    )
          )
        ]
