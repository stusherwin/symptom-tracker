module Textarea exposing (view)

import Array exposing (isEmpty)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)


view : String -> String -> String -> (String -> msg) -> Bool -> { showFilled : Bool } -> Html msg
view id class value toMsg isValid { showFilled } =
    let
        isEmpty =
            String.isEmpty value
    in
    div
        [ A.class "rounded border-4"
        , A.class class
        , classList
            [ ( "border-black border-opacity-50 hover:border-opacity-80 focus-within:border-opacity-80", isEmpty )
            , ( "border-black border-opacity-50 hover:border-opacity-80 focus-within:border-opacity-80", not showFilled && isValid && not isEmpty )
            , ( "border-blue-500 border-opacity-70 hover:border-opacity-100 focus-within:border-opacity-100", showFilled && isValid && not isEmpty )
            , ( "border-red-500 border-opacity-70 hover:border-opacity-100 focus-within:border-opacity-100", not isValid && not isEmpty )
            ]
        ]
        [ textarea
            [ A.id id
            , A.class "block w-full h-full py-1 px-2"
            , A.value value
            , onInput toMsg
            ]
            []
        ]
