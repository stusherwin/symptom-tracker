port module Ports exposing (..)

import Json.Encode as Encode


port setUserData : Encode.Value -> Cmd msg


port toggleElementFullScreen : String -> Cmd msg


port fullScreenChanged : (Bool -> msg) -> Sub msg
