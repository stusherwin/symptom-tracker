module ExpandedDataSet exposing (..)

import Array exposing (Array)
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Dictx
import UserData.Chartable as C exposing (Chartable)
import UserData.ChartableId exposing (ChartableId)
import UserData.LineChart as LC exposing (LineChart(..))
import UserData.LineChartId as LCId exposing (LineChartId)
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId exposing (TrackableId)


type alias ExpandedDataSet =
    { name : String
    , isInverted : Bool
    , data : List ( String, Dict Int Float, Float )
    }


fromChartable : Chartable -> ExpandedDataSet
fromChartable chartable =
    { name = C.name chartable
    , isInverted = C.isInverted chartable
    , data =
        C.sum chartable
            |> List.map
                (\( _, ( t, m ) ) ->
                    ( T.question t, T.onlyFloatData t, m )
                )
    }


fromTrackable : Trackable -> Float -> Bool -> ExpandedDataSet
fromTrackable trackable multiplier isInverted =
    { name = T.question trackable
    , isInverted = isInverted
    , data = [ ( T.question trackable, T.onlyFloatData trackable, multiplier ) ]
    }
