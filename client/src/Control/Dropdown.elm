module Control.Dropdown exposing (viewColour, viewColourWithText, viewIcon, viewText)

import Colour exposing (Colour)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Svg.Icon as Icon exposing (IconType(..), icon)


view : String -> (Maybe a -> msg) -> (a -> String) -> (String -> Maybe a) -> List ( ( a, Bool ), Html msg ) -> Maybe a -> { unselectedItemClass : String, selectedItemClass : String, showFilled : Bool } -> Html msg
view class toMsg toString fromString options selectedValue { unselectedItemClass, selectedItemClass, showFilled } =
    let
        isEmpty =
            case selectedValue of
                Just _ ->
                    False

                _ ->
                    True
    in
    node "dropdown-list"
        [ A.class "block relative z-0 rounded border-4"
        , A.class class
        , classList
            [ ( "border-black border-opacity-50 hover:border-opacity-80 focus-within:border-opacity-80", isEmpty )
            , ( "border-black border-opacity-50 hover:border-opacity-80 focus-within:border-opacity-80", not showFilled && not isEmpty )
            , ( "border-blue-500 border-opacity-70 hover:border-opacity-100 focus-within:border-opacity-100", showFilled && not isEmpty )
            ]
        , attribute "selected-option-classes" selectedItemClass
        , attribute "unselected-option-classes" unselectedItemClass
        , attribute "selected-value" (Maybe.withDefault "" <| Maybe.map toString selectedValue)
        , on "dropdown-list-option-selected" (Decode.map (toMsg << fromString) (Decode.field "detail" Decode.string))
        ]
        [ div [ A.class "group w-full h-full" ]
            [ input
                [ type_ "text"
                , A.class "options-input block w-full h-full py-1 px-2 text-lg font-bold"
                ]
                []
            , div
                [ A.class "options-selected-display absolute top-0 bottom-0 left-0 right-8"
                , A.class unselectedItemClass
                ]
                []
            , div [ A.class "absolute right-0 top-0 bottom-0 w-8 flex justify-center items-center bg-gray-200 group-hover:bg-gray-300" ] [ icon "w-6 h-6" SolidAngleDown ]
            ]
        , ul [ tabindex -1, A.class "group options-parent hidden-visually absolute bg-white border border-gray-800 max-h-80 overflow-y-auto mt-1 -left-1 -right-1" ] <|
            li [ tabindex -1, A.class "options-none hidden-all py-5 px-2 text-lg italic" ] []
                :: List.map
                    (\( ( v, enabled ), html ) ->
                        li
                            [ tabindex -1
                            , attribute "data-value" (toString v)
                            , A.class "cursor-default"
                            , A.classList <|
                                [ ( "options-selectable", enabled )
                                , ( "text-gray-300", not enabled )
                                ]
                                    ++ List.map (\s -> ( "focus:" ++ s, enabled )) (String.split " " selectedItemClass)
                                    ++ List.map (\s -> ( "hover:" ++ s, enabled )) (String.split " " selectedItemClass)
                                    ++ List.map (\s -> ( "group-hover:" ++ s, enabled )) (String.split " " unselectedItemClass)
                            ]
                            [ html ]
                    )
                    options
        ]


viewText : String -> (Maybe a -> msg) -> (a -> String) -> (String -> Maybe a) -> List ( ( a, Bool ), String ) -> Maybe a -> { showFilled : Bool } -> Html msg
viewText class toMsg toString fromString options selectedValue { showFilled } =
    view class
        toMsg
        toString
        fromString
        (List.map (Tuple.mapSecond (\t -> div [ A.class "py-1 px-2 text-lg font-bold" ] [ text t ])) options)
        selectedValue
        { unselectedItemClass = "bg-white text-black"
        , selectedItemClass = "bg-gray-800 text-white"
        , showFilled = showFilled
        }


viewColour : String -> (Maybe Colour -> msg) -> Maybe Colour -> { showFilled : Bool } -> Html msg
viewColour class toMsg selectedValue { showFilled } =
    view (class ++ " w-20 h-12")
        toMsg
        Colour.toString
        Colour.fromString
        (List.map
            (\c ->
                ( ( c, True )
                , div [ A.class "p-1 text-lg font-bold flex items-center" ]
                    [ div [ A.class "w-8 h-8 rounded-full flex-shrink-0 flex-grow-0 border-2", Colour.class "bg" c, Colour.classUp "border" c ] []
                    ]
                )
            )
            Colour.userSelectable
        )
        selectedValue
        { unselectedItemClass = "bg-white text-black"
        , selectedItemClass = "bg-gray-800 text-white"
        , showFilled = showFilled
        }


viewColourWithText : String -> (Maybe Colour -> msg) -> Maybe Colour -> { showFilled : Bool } -> Html msg
viewColourWithText dropdownClass toMsg selectedValue { showFilled } =
    view (dropdownClass ++ " w-20 h-12")
        toMsg
        Colour.toString
        Colour.fromString
        (List.map
            (\c ->
                ( ( c, True )
                , div [ class "p-1 text-lg font-bold flex items-center" ]
                    [ div [ class "w-8 h-8 mr-2 rounded-full flex-shrink-0 flex-grow-0 border-2", Colour.class "bg" c, Colour.classUp "border" c ] []
                    , text (Colour.toUserString c)
                    ]
                )
            )
            Colour.userSelectable
        )
        selectedValue
        { unselectedItemClass = "bg-white text-black"
        , selectedItemClass = "bg-gray-800 text-white"
        , showFilled = showFilled
        }


viewIcon : String -> (Maybe IconType -> msg) -> Maybe IconType -> { showFilled : Bool } -> Html msg
viewIcon dropdownClass toMsg selectedValue { showFilled } =
    view (dropdownClass ++ " w-20 h-12")
        toMsg
        Icon.toString
        Icon.fromString
        (List.map
            (\iconType ->
                ( ( iconType, True )
                , div [ class "p-1 pr-2 text-lg font-bold flex items-center" ]
                    [ icon "w-8 h-8" iconType
                    ]
                )
            )
            Icon.userSelectable
        )
        selectedValue
        { unselectedItemClass = "bg-gray-800 text-white"
        , selectedItemClass = "bg-white text-black"
        , showFilled = showFilled
        }
