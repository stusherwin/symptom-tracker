module Colour exposing (Colour(..), all, class, classDown, classUp, decode, encode, fromString, toString, toUserString, userSelectable)

import Html exposing (Attribute)
import Html.Attributes as Html exposing (class)
import Json.Decode as D
import Json.Encode as E


type Colour
    = Black
    | White
    | Gray
    | MidGray
    | LightGray
    | LighterGray
    | LightestGray
    | Red
    | Orange
    | Amber
    | Yellow
    | Lime
    | Green
    | Emerald
    | Teal
    | Cyan
    | LightBlue
    | Blue
    | Indigo
    | Violet
    | Purple
    | Fuchsia
    | Pink
    | Rose


toString : Colour -> String
toString colour =
    case colour of
        Black ->
            "black"

        White ->
            "white"

        Gray ->
            "gray"

        MidGray ->
            "midGray"

        LightGray ->
            "lightGray"

        LighterGray ->
            "lighterGray"

        LightestGray ->
            "lightestGray"

        Red ->
            "red"

        Orange ->
            "orange"

        Amber ->
            "amber"

        Yellow ->
            "yellow"

        Lime ->
            "lime"

        Green ->
            "green"

        Emerald ->
            "emerald"

        Teal ->
            "teal"

        Cyan ->
            "cyan"

        LightBlue ->
            "lightBlue"

        Blue ->
            "blue"

        Indigo ->
            "indigo"

        Violet ->
            "violet"

        Purple ->
            "purple"

        Fuchsia ->
            "fuchsia"

        Pink ->
            "pink"

        Rose ->
            "rose"


toUserString : Colour -> String
toUserString =
    let
        splitWords chars =
            case chars of
                [] ->
                    []

                [ c ] ->
                    [ c ]

                c1 :: c2 :: cs ->
                    if Char.isLower c1 && Char.isUpper c2 then
                        c1 :: ' ' :: c2 :: splitWords cs

                    else
                        c1 :: c2 :: splitWords cs

        firstUpper chars =
            case chars of
                [] ->
                    []

                c :: cs ->
                    Char.toUpper c :: cs
    in
    String.fromList << firstUpper << splitWords << String.toList << toString


fromString : String -> Maybe Colour
fromString str =
    case str of
        "black" ->
            Just Black

        "white" ->
            Just White

        "gray" ->
            Just Gray

        "midGray" ->
            Just MidGray

        "lightGray" ->
            Just LightGray

        "lighterGray" ->
            Just LighterGray

        "lightestGray" ->
            Just LightestGray

        "red" ->
            Just Red

        "orange" ->
            Just Orange

        "amber" ->
            Just Amber

        "yellow" ->
            Just Yellow

        "lime" ->
            Just Lime

        "green" ->
            Just Green

        "emerald" ->
            Just Emerald

        "teal" ->
            Just Teal

        "cyan" ->
            Just Cyan

        "lightBlue" ->
            Just LightBlue

        "blue" ->
            Just Blue

        "indigo" ->
            Just Indigo

        "violet" ->
            Just Violet

        "purple" ->
            Just Purple

        "fuchsia" ->
            Just Fuchsia

        "pink" ->
            Just Pink

        "rose" ->
            Just Rose

        _ ->
            Nothing


all : List Colour
all =
    [ Black
    , White
    , Gray
    , MidGray
    , LightGray
    , LighterGray
    , LightestGray
    , Red
    , Orange
    , Amber
    , Yellow
    , Lime
    , Green
    , Emerald
    , Teal
    , Cyan
    , LightBlue
    , Blue
    , Indigo
    , Violet
    , Purple
    , Fuchsia
    , Pink
    , Rose
    ]


userSelectable : List Colour
userSelectable =
    [ Red
    , Orange
    , Amber
    , Yellow
    , Lime
    , Green
    , Emerald
    , Teal
    , Cyan
    , LightBlue
    , Blue
    , Indigo
    , Violet
    , Purple
    , Fuchsia
    , Pink
    , Rose
    , Gray
    ]


class : String -> Colour -> Attribute msg
class prefix colour =
    Html.class <|
        prefix
            ++ "-"
            ++ (case colour of
                    Black ->
                        "gray-800"

                    White ->
                        "white"

                    MidGray ->
                        "gray-300"

                    LightGray ->
                        "gray-200"

                    LighterGray ->
                        "gray-100"

                    LightestGray ->
                        "gray-50"

                    _ ->
                        toString colour ++ "-300"
               )


classUp : String -> Colour -> Attribute msg
classUp prefix colour =
    Html.class <|
        prefix
            ++ "-"
            ++ (case colour of
                    Black ->
                        "gray-900"

                    White ->
                        "gray-50"

                    MidGray ->
                        "gray-400"

                    LightGray ->
                        "gray-300"

                    LighterGray ->
                        "gray-200"

                    LightestGray ->
                        "gray-100"

                    _ ->
                        toString colour ++ "-400"
               )


classDown : String -> Colour -> Attribute msg
classDown prefix colour =
    Html.class <|
        prefix
            ++ "-"
            ++ (case colour of
                    Black ->
                        "gray-700"

                    White ->
                        "white"

                    MidGray ->
                        "gray-200"

                    LightGray ->
                        "gray-100"

                    LighterGray ->
                        "gray-50"

                    LightestGray ->
                        "white"

                    _ ->
                        toString colour ++ "-200"
               )


decode : D.Decoder Colour
decode =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just col ->
                        D.succeed col

                    _ ->
                        D.fail <| "Invalid Colour: " ++ str
            )


encode : Colour -> E.Value
encode =
    E.string << toString
