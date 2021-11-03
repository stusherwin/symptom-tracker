module Controls exposing (ButtonColour(..), button, circleButton, colourDropdown, iconButton, iconDropdown, textDropdown, textarea, textbox)

import Colour exposing (Colour)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Svg.Icon as Icon exposing (IconType(..))


type ButtonColour
    = ButtonGrey
    | ButtonBlue


button : String -> ButtonColour -> msg -> IconType -> String -> Bool -> Html msg
button class colour toMsg icon text enabled =
    div
        [ A.class "rounded border-4"
        , A.class class
        , classList
            [ ( "border-black", colour == ButtonGrey )
            , ( "border-blue-600", colour == ButtonBlue )
            , ( "border-opacity-80 hover:border-opacity-100 focus-within:border-opacity-100", enabled )
            , ( "border-opacity-40", not enabled )
            ]
        ]
        [ H.button
            [ A.class "w-full h-full px-2 py-1 text-white flex items-center text-center focus:outline-none"
            , classList
                [ ( "cursor-default bg-opacity-80 hover:bg-opacity-90", enabled )
                , ( "cursor-default bg-opacity-30", not enabled )
                , ( "bg-gray-800", colour == ButtonGrey )
                , ( "bg-blue-500", colour == ButtonBlue )
                ]
            , A.disabled (not enabled)
            , onClick toMsg
            ]
            [ Icon.icon "w-4 h-4 mr-2" icon
            , H.text text
            ]
        ]


iconButton : String -> ButtonColour -> msg -> IconType -> Bool -> Html msg
iconButton class colour toMsg icon enabled =
    div
        [ A.class "rounded border-4"
        , A.class class
        , classList
            [ ( "border-black", colour == ButtonGrey )
            , ( "border-blue-600", colour == ButtonBlue )
            , ( "border-opacity-80 hover:border-opacity-100 focus-within:border-opacity-100", enabled )
            , ( "border-opacity-40", not enabled )
            ]
        ]
        [ H.button
            [ A.class "w-full h-full p-2 text-white flex items-center text-center focus:outline-none"
            , classList
                [ ( "cursor-default bg-opacity-80 hover:bg-opacity-90", enabled )
                , ( "cursor-default bg-opacity-30", not enabled )
                , ( "bg-gray-800", colour == ButtonGrey )
                , ( "bg-blue-500", colour == ButtonBlue )
                ]
            , A.disabled (not enabled)
            , onClick toMsg
            ]
            [ Icon.icon "w-8 h-8" icon
            ]
        ]


circleButton : String -> ButtonColour -> msg -> String -> Bool -> Html msg
circleButton class colour toMsg text enabled =
    div
        [ A.class "rounded-full border-4"
        , A.class class
        , classList
            [ ( "border-black", colour == ButtonGrey )
            , ( "border-blue-600", colour == ButtonBlue )
            , ( "border-opacity-80 hover:border-opacity-100 focus-within:border-opacity-100", enabled )
            , ( "border-opacity-40", not enabled )
            ]
        ]
        [ H.button
            [ A.class "w-10 h-10 rounded-full p-2 text-white text-xl font-bold flex items-center justify-center text-center"
            , classList
                [ ( "cursor-default bg-opacity-80 hover:bg-opacity-90", enabled )
                , ( "cursor-default bg-opacity-30", not enabled )
                , ( "bg-gray-800", colour == ButtonGrey )
                , ( "bg-blue-500", colour == ButtonBlue )
                ]
            , A.disabled (not enabled)
            , onClick toMsg
            ]
            [ H.text text
            ]
        ]


dropdown : String -> (Maybe a -> msg) -> (a -> String) -> (String -> Maybe a) -> List ( ( a, Bool ), Html msg ) -> Maybe (Html msg) -> Maybe a -> { unselectedItemClass : String, selectedItemClass : String, showFilled : Bool, darkBorder : Bool } -> Html msg
dropdown class toMsg toString fromString options optionsNone selectedValue { unselectedItemClass, selectedItemClass, showFilled, darkBorder } =
    let
        isEmpty =
            case selectedValue of
                Just _ ->
                    False

                _ ->
                    True

        option value enabled html =
            li
                [ tabindex -1
                , attribute "data-value" value
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
    in
    node "dropdown-list"
        [ A.class "block relative z-0 rounded border-4"
        , A.class class
        , classList
            [ ( "border-black", isEmpty )
            , ( "border-black", not isEmpty && not showFilled )
            , ( "border-blue-600", not isEmpty && showFilled )
            , ( "border-opacity-80 hover:border-opacity-100 focus-within:border-opacity-100", darkBorder )
            , ( "border-opacity-50 hover:border-opacity-80 focus-within:border-opacity-80", not darkBorder )
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
                [ A.class "options-selected-display absolute top-0 bottom-0 left-0 right-8 whitespace-nowrap overflow-hidden"
                , A.class unselectedItemClass
                ]
                []
            , div [ A.class "absolute right-0 top-0 bottom-0 w-8 flex justify-center items-center bg-gray-200 group-hover:bg-gray-300" ] [ Icon.icon "w-6 h-6" SolidAngleDown ]
            ]
        , ul [ tabindex -1, A.class "group options-parent hidden-visually absolute bg-white border border-gray-800 max-h-80 overflow-y-auto mt-1 -left-1 -right-1" ] <|
            li [ tabindex -1, A.class "options-not-found hidden-all py-5 px-2 text-lg italic" ] []
                :: (case optionsNone of
                        Just html ->
                            [ option "" True html ]

                        _ ->
                            []
                   )
                ++ (options
                        |> List.map
                            (\( ( v, enabled ), html ) ->
                                option (toString v) enabled html
                            )
                   )
        ]


textDropdown : String -> (Maybe a -> msg) -> (a -> String) -> (String -> Maybe a) -> List ( ( a, Bool ), String ) -> Maybe String -> Maybe a -> { showFilled : Bool } -> Html msg
textDropdown class toMsg toString fromString options optionsNone selectedValue { showFilled } =
    dropdown class
        toMsg
        toString
        fromString
        (List.map (Tuple.mapSecond (\t -> div [ A.class "py-1 px-2 font-bold flex items-center" ] [ span [] [ text t ] ])) options)
        (optionsNone |> Maybe.map (\t -> div [ A.class "py-1 px-2 font-bold flex items-center" ] [ span [] [ text t ] ]))
        selectedValue
        { unselectedItemClass = "bg-white text-black"
        , selectedItemClass = "bg-gray-800 text-white"
        , showFilled = showFilled
        , darkBorder = False
        }


colourDropdown : String -> (Maybe Colour -> msg) -> Maybe Colour -> { showFilled : Bool } -> Html msg
colourDropdown class toMsg selectedValue { showFilled } =
    dropdown (class ++ " w-20 h-12")
        toMsg
        Colour.toString
        Colour.fromString
        (List.map
            (\c ->
                ( ( c, True )
                , div [ A.class "p-1 font-bold flex items-center" ]
                    [ div [ A.class "w-8 h-8 rounded-full flex-shrink-0 flex-grow-0 border-2", Colour.bgClass c, Colour.borderClassDarker c ] []
                    ]
                )
            )
            Colour.userSelectable
        )
        Nothing
        selectedValue
        { unselectedItemClass = "bg-white text-black"
        , selectedItemClass = "bg-gray-800 text-white"
        , showFilled = showFilled
        , darkBorder = False
        }


iconDropdown : String -> (Maybe IconType -> msg) -> Maybe IconType -> { showFilled : Bool } -> Html msg
iconDropdown dropdownClass toMsg selectedValue { showFilled } =
    dropdown (dropdownClass ++ " w-20 h-12")
        toMsg
        Icon.toString
        Icon.fromString
        (List.map
            (\iconType ->
                ( ( iconType, True )
                , div [ class "p-1 pr-2 text-lg font-bold flex items-center" ]
                    [ Icon.icon "w-8 h-8" iconType
                    ]
                )
            )
            Icon.userSelectable
        )
        Nothing
        selectedValue
        { unselectedItemClass = "bg-gray-800 text-white"
        , selectedItemClass = "bg-white text-black"
        , showFilled = showFilled
        , darkBorder = True
        }


textbox : List (Attribute msg) -> List (Attribute msg) -> String -> { isValid : Bool, isRequired : Bool, isPristine : Bool } -> (String -> msg) -> Html msg
textbox outerAttributes inputAttributes value { isValid, isRequired, isPristine } toMsg =
    let
        isEmpty =
            String.isEmpty value
    in
    div
        ([ A.class "rounded border-4 h-10 border-opacity-50 hover:border-opacity-80 focus-within:border-opacity-80"
         , classList
            [ ( "border-black", isEmpty && (not isRequired || isPristine) || not isEmpty && isValid )
            , ( "border-red-600", isEmpty && isRequired && not isPristine || not isEmpty && not isValid )
            ]
         ]
            ++ outerAttributes
        )
        [ input
            ([ type_ "text"
             , A.class "block w-full h-full py-1 px-2 font-bold"
             , A.value value
             , onInput toMsg
             ]
                ++ inputAttributes
            )
            []
        ]


textarea : String -> String -> String -> (String -> msg) -> Bool -> { showFilled : Bool } -> Html msg
textarea id class value toMsg isValid { showFilled } =
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
        [ H.textarea
            [ A.id id
            , A.class "block w-full h-full py-1 px-2"
            , A.value value
            , onInput toMsg
            ]
            []
        ]
