module DataSet exposing (..)

import Array exposing (Array)
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Extra.Dict as Dict
import UserData.Chartable as C exposing (Chartable)
import UserData.ChartableId exposing (ChartableId)
import UserData.LineChart as LC exposing (LineChart(..))
import UserData.LineChartId as LCId exposing (LineChartId)
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId exposing (TrackableId)


type alias DataSet =
    { name : String
    , colour : Colour
    , dataPoints : Dict Int Float
    , isVisible : Bool
    , isInverted : Bool
    , expandedData : List ( String, Dict Int Float, Float )
    }


fromChartable : Chartable -> Bool -> DataSet
fromChartable c isVisible =
    let
        invert d =
            case List.maximum <| Dict.values d of
                Just max ->
                    d |> Dict.map (\_ v -> max - v)

                _ ->
                    d
    in
    { name = C.name c
    , colour = C.colour c
    , dataPoints =
        C.sum c
            |> List.map
                (\( _, ( trackable, multiplier ) ) ->
                    trackable
                        |> T.onlyFloatData
                        |> Dict.map (\_ v -> v * multiplier)
                )
            |> List.foldl (Dict.unionWith (\v1 v2 -> v1 + v2)) Dict.empty
            |> (if C.isInverted c then
                    invert

                else
                    identity
               )
    , isVisible = isVisible
    , isInverted = C.isInverted c
    , expandedData =
        C.sum c
            |> List.map
                (\( _, ( t, m ) ) ->
                    ( T.question t, T.onlyFloatData t, m )
                )
    }


fromTrackable : Trackable -> Float -> Bool -> Bool -> DataSet
fromTrackable t multiplier isInverted isVisible =
    let
        invert rs =
            case List.maximum <| Dict.values rs of
                Just max ->
                    rs |> Dict.map (\_ v -> max - v)

                _ ->
                    rs
    in
    { name = T.question t
    , colour = T.colour t
    , dataPoints =
        T.onlyFloatData t
            |> Dict.map (\_ v -> v * multiplier)
            |> (if isInverted then
                    invert

                else
                    identity
               )
    , isVisible = isVisible
    , isInverted = isInverted
    , expandedData = [ ( T.question t, T.onlyFloatData t, multiplier ) ]
    }


maxValue : Array DataSet -> Float
maxValue =
    Maybe.withDefault 0
        << List.maximum
        << List.filterMap (List.maximum << Dict.values << .dataPoints)
        << Array.toList


startDate : Date -> Array DataSet -> Date
startDate today data =
    let
        minDate =
            Date.fromRataDie
                << Maybe.withDefault (Date.toRataDie today)
                << List.minimum
                << List.filterMap (List.head << Dict.keys << .dataPoints)
                << Array.toList
            <|
                data

        fullWeeks =
            ceiling <| toFloat (Date.diff Days minDate today) / 7
    in
    Date.add Weeks -fullWeeks today
