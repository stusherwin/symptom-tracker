module Control.Button exposing (ButtonColour(..), view, viewCircle, viewIcon)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg.Icon as I exposing (IconType(..))


type ButtonColour
    = Grey
    | Blue


view : String -> ButtonColour -> msg -> IconType -> String -> Bool -> Html msg
view class colour toMsg icon text enabled =
    div
        [ A.class "rounded border-4"
        , A.class class
        , classList
            [ ( "border-black border-opacity-50", colour == Grey )
            , ( "hover:border-opacity-80 focus-within:border-opacity-80", colour == Grey && enabled )
            , ( "border-blue-500 border-opacity-70", colour == Blue )
            , ( "hover:border-opacity-100 focus-within:border-opacity-100", colour == Blue && enabled )
            ]
        ]
        [ button
            [ A.class "w-full h-full px-2 py-1 text-white flex items-center text-center focus:outline-none"
            , onClick toMsg
            , classList
                [ ( "cursor-default bg-opacity-30", not enabled )
                , ( "bg-gray-800", colour == Grey )
                , ( "bg-blue-500", colour == Blue )
                ]
            , A.disabled (not enabled)
            ]
            [ I.icon "w-4 h-4 mr-2" icon
            , H.text text
            ]
        ]


viewIcon : String -> msg -> IconType -> Html msg
viewIcon class toMsg icon =
    div
        [ A.class "rounded border-4"
        , A.class class
        ]
        [ button
            [ A.class "w-full h-full p-2 text-white flex items-center text-center"
            , onClick toMsg
            ]
            [ I.icon "w-8 h-8" icon
            ]
        ]


viewCircle : String -> msg -> String -> Html msg
viewCircle class toMsg text =
    div
        [ A.class "w-10 h-10 rounded-full text-lg font-bold border-4"
        , A.class class
        ]
        [ button
            [ A.class "w-full h-full p-2 text-white flex items-center text-center"
            , onClick toMsg
            ]
            [ H.text text
            ]
        ]
