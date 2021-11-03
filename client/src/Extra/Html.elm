module Extra.Html exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    let
        alwaysPreventDefault m =
            ( m, True )
    in
    preventDefaultOn "click" (Decode.map alwaysPreventDefault (Decode.succeed msg))


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    let
        alwaysStopPropagation m =
            ( m, True )
    in
    stopPropagationOn "click" (Decode.map alwaysStopPropagation (Decode.succeed msg))


onCheckStopPropagation : msg -> Attribute msg
onCheckStopPropagation msg =
    let
        alwaysStopPropagation m =
            ( m, True )
    in
    stopPropagationOn "change" (Decode.map alwaysStopPropagation (Decode.succeed msg))
