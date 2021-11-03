module UserData.LineChart exposing (LineChart, LineChartDict, LineChartElement(..), New, StateDataSet(..), add, addChartable, addTrackable, buildDict, dataSets, decode, decodeV5, deleteData, encode, fillLines, moveDataDown, moveDataUp, name, replaceTrackable, replaceTrackableWithChartable, setFillLines, setName, setTrackableInverted, setTrackableMultiplier, toggleDataVisible, updateChartable, updateTrackable)

import Array exposing (Array)
import Extra.Array as Array
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
    , dataSets : Array ( LineChartElement, Bool )
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


type LineChartElement
    = ChartableElement
        { chartableId : ChartableId
        , chartable : Chartable
        }
    | TrackableElement
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


dataSets : LineChart -> Array ( LineChartElement, Bool )
dataSets (LineChart c) =
    c.dataSets


add : ChartableDict -> TrackableDict -> New -> LineChartDict -> Result String ( ( LineChartId, LineChart ), LineChartDict )
add chartables trackables c dict =
    let
        chart =
            build chartables trackables c
    in
    dict |> IdDict.add chart |> Result.map (\( id, dict_ ) -> ( ( id, chart ), dict_ ))


setName : String -> LineChart -> Result String LineChart
setName n (LineChart c) =
    Ok <| LineChart { c | name = n }


setFillLines : Bool -> LineChart -> Result String LineChart
setFillLines fl (LineChart c) =
    Ok <| LineChart { c | fillLines = fl }


addChartable : ChartableId -> Chartable -> LineChart -> Result String LineChart
addChartable id chartable (LineChart c) =
    Ok <| LineChart { c | dataSets = c.dataSets |> Array.push ( ChartableElement { chartableId = id, chartable = chartable }, True ) }


addTrackable : TrackableId -> Trackable -> LineChart -> Result String LineChart
addTrackable id trackable (LineChart c) =
    Ok <| LineChart { c | dataSets = c.dataSets |> Array.push ( TrackableElement { trackableId = id, trackable = trackable, multiplier = 1, isInverted = False }, True ) }


replaceTrackableWithChartable : Int -> ChartableId -> Chartable -> LineChart -> Result String LineChart
replaceTrackableWithChartable i id chartable (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Trackable with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <| LineChart { c | dataSets = c.dataSets |> Array.set i ( ChartableElement { chartableId = id, chartable = chartable }, True ) }


replaceTrackable : Int -> TrackableId -> Trackable -> Float -> Bool -> LineChart -> Result String LineChart
replaceTrackable i id trackable multiplier isInverted (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Trackable with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <| LineChart { c | dataSets = c.dataSets |> Array.set i ( TrackableElement { trackableId = id, trackable = trackable, multiplier = multiplier, isInverted = isInverted }, True ) }


updateTrackable : TrackableId -> Trackable -> LineChart -> Result String LineChart
updateTrackable id trackable (LineChart c) =
    Ok <|
        LineChart <|
            { c
                | dataSets =
                    c.dataSets
                        |> (Array.map <|
                                Tuple.mapFirst <|
                                    \ds ->
                                        case ds of
                                            TrackableElement e ->
                                                if e.trackableId == id then
                                                    TrackableElement { e | trackable = trackable }

                                                else
                                                    TrackableElement e

                                            x ->
                                                x
                           )
            }


updateChartable : ChartableId -> Chartable -> LineChart -> Result String LineChart
updateChartable id chartable (LineChart c) =
    Ok <|
        LineChart <|
            { c
                | dataSets =
                    c.dataSets
                        |> (Array.map <|
                                Tuple.mapFirst <|
                                    \ds ->
                                        case ds of
                                            ChartableElement e ->
                                                if e.chartableId == id then
                                                    ChartableElement { e | chartable = chartable }

                                                else
                                                    ChartableElement e

                                            x ->
                                                x
                           )
            }


setTrackableInverted : Int -> Bool -> LineChart -> Result String LineChart
setTrackableInverted i isInverted (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Trackable with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <|
                LineChart
                    { c
                        | dataSets =
                            c.dataSets
                                |> Array.update i
                                    (\d ->
                                        case d of
                                            ( TrackableElement t, visible ) ->
                                                ( TrackableElement { t | isInverted = isInverted }, visible )

                                            _ ->
                                                d
                                    )
                    }


setTrackableMultiplier : Int -> Float -> LineChart -> Result String LineChart
setTrackableMultiplier i multiplier (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Trackable with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <|
                LineChart
                    { c
                        | dataSets =
                            c.dataSets
                                |> Array.update i
                                    (\d ->
                                        case d of
                                            ( TrackableElement t, visible ) ->
                                                ( TrackableElement { t | multiplier = multiplier }, visible )

                                            _ ->
                                                d
                                    )
                    }


deleteData : Int -> LineChart -> Result String LineChart
deleteData i (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Data with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <| LineChart { c | dataSets = c.dataSets |> Array.delete i }


toggleDataVisible : Int -> LineChart -> Result String LineChart
toggleDataVisible i (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Data with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <| LineChart { c | dataSets = c.dataSets |> Array.update i (Tuple.mapSecond not) }


moveDataUp : Int -> LineChart -> Result String LineChart
moveDataUp i (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Data with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <| LineChart { c | dataSets = c.dataSets |> Array.swap i (i - 1) }


moveDataDown : Int -> LineChart -> Result String LineChart
moveDataDown i (LineChart c) =
    case c.dataSets |> Array.get i of
        Nothing ->
            Err <| "Data with index " ++ String.fromInt i ++ " does not exist in LineChart"

        Just _ ->
            Ok <| LineChart { c | dataSets = c.dataSets |> Array.swap i (i + 1) }


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
                                    |> Maybe.map (\chartable -> ( ChartableElement { chartableId = chartableId, chartable = chartable }, visible ))

                            StateTrackable { trackableId, multiplier, isInverted } ->
                                trackables
                                    |> IdDict.get trackableId
                                    |> Maybe.map
                                        (\trackable ->
                                            ( TrackableElement
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
                            ChartableElement { chartableId } ->
                                StateChartable { chartableId = chartableId }

                            TrackableElement { trackableId, multiplier, isInverted } ->
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
