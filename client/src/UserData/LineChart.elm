module UserData.LineChart exposing (LineChart, LineChartData(..), LineChartDict, TrackableData, addChartable, addTrackable, decode, decodeV5, deleteData, encode, moveDataDown, moveDataUp, replaceTrackable, replaceTrackableWithChartable, setFillLines, setName, setTrackableInverted, setTrackableMultiplier, toggleDataVisible)

import Array exposing (Array)
import Arrayx
import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import Listx
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChartId exposing (LineChartId)
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias LineChart =
    { name : String
    , fillLines : Bool
    , data : Array ( LineChartData, Bool )
    }


type LineChartData
    = Chartable ChartableId
    | Trackable TrackableData


type alias TrackableData =
    { id : TrackableId
    , multiplier : Float
    , inverted : Bool
    }


type alias LineChartDict =
    IdDict LineChartId LineChart


setName : String -> LineChart -> LineChart
setName name c =
    { c | name = name }


setFillLines : Bool -> LineChart -> LineChart
setFillLines fl c =
    { c | fillLines = fl }


addChartable : ChartableId -> LineChart -> LineChart
addChartable id c =
    { c | data = c.data |> Array.push ( Chartable id, True ) }


addTrackable : TrackableId -> LineChart -> LineChart
addTrackable id c =
    { c | data = c.data |> Array.push ( Trackable { id = id, multiplier = 1, inverted = False }, True ) }


replaceTrackableWithChartable : Int -> ChartableId -> LineChart -> LineChart
replaceTrackableWithChartable i id c =
    { c | data = c.data |> Array.set i ( Chartable id, True ) }


replaceTrackable : Int -> TrackableId -> Float -> Bool -> LineChart -> LineChart
replaceTrackable i id multiplier inverted c =
    { c | data = c.data |> Array.set i ( Trackable { id = id, multiplier = multiplier, inverted = inverted }, True ) }


setTrackableInverted : Int -> Bool -> LineChart -> LineChart
setTrackableInverted i inverted c =
    { c
        | data =
            c.data
                |> Arrayx.update i
                    (\d ->
                        case d of
                            ( Trackable t, visible ) ->
                                ( Trackable { t | inverted = inverted }, visible )

                            _ ->
                                d
                    )
    }


setTrackableMultiplier : Int -> Float -> LineChart -> LineChart
setTrackableMultiplier i multiplier c =
    { c
        | data =
            c.data
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
deleteData i c =
    { c | data = c.data |> Arrayx.delete i }


toggleDataVisible : Int -> LineChart -> LineChart
toggleDataVisible i c =
    { c | data = c.data |> Arrayx.update i (Tuple.mapSecond not) }


moveDataUp : Int -> LineChart -> LineChart
moveDataUp i c =
    { c | data = c.data |> Arrayx.swap i (i - 1) }


moveDataDown : Int -> LineChart -> LineChart
moveDataDown i c =
    { c | data = c.data |> Arrayx.swap i (i + 1) }


decode : D.Decoder LineChart
decode =
    D.map3
        (\name fillLines chartables ->
            { name = name
            , data = chartables |> List.map (Tuple.mapFirst Chartable) |> Array.fromList
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


decodeV5 : D.Decoder LineChart
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


decodeData : D.Decoder LineChartData
decodeData =
    D.oneOf
        [ D.map Chartable ChartableId.decode
        , D.map Trackable <|
            D.map3
                TrackableData
                (D.field "id" TrackableId.decode)
                (D.field "multiplier" D.float)
                (D.field "inverted" D.bool)
        ]


encodeData : LineChartData -> E.Value
encodeData d =
    case d of
        Chartable id ->
            ChartableId.encode id

        Trackable { id, multiplier, inverted } ->
            E.object
                [ ( "id", TrackableId.encode id )
                , ( "multiplier", E.float multiplier )
                , ( "inverted", E.bool inverted )
                ]


encode : LineChart -> E.Value
encode c =
    E.object
        [ ( "name", E.string c.name )
        , ( "fillLines", E.bool c.fillLines )
        , ( "data"
          , c.data
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
