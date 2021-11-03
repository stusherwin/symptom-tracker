module Colour exposing (Colour(..), all, bgClass, bgClassLighter, borderClassDarker, decode, encode, fromString, toString, toUserString, userSelectable)

import Html exposing (Attribute)
import Html.Attributes as Html
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


bgClass : Colour -> Attribute msg
bgClass colour =
    Html.class <|
        case colour of
            Black ->
                "bg-gray-800"

            White ->
                "bg-white"

            MidGray ->
                "bg-gray-300"

            LightGray ->
                "bg-gray-200"

            LighterGray ->
                "bg-gray-100"

            LightestGray ->
                "bg-gray-50"

            Gray ->
                "bg-gray-300"

            Red ->
                "bg-red-300"

            Orange ->
                "bg-orange-300"

            Amber ->
                "bg-amber-300"

            Yellow ->
                "bg-yellow-300"

            Lime ->
                "bg-lime-300"

            Green ->
                "bg-green-300"

            Emerald ->
                "bg-emerald-300"

            Teal ->
                "bg-teal-300"

            Cyan ->
                "bg-cyan-300"

            LightBlue ->
                "bg-lightBlue-300"

            Blue ->
                "bg-blue-300"

            Indigo ->
                "bg-indigo-300"

            Violet ->
                "bg-violet-300"

            Purple ->
                "bg-purple-300"

            Fuchsia ->
                "bg-fuchsia-300"

            Pink ->
                "bg-pink-300"

            Rose ->
                "bg-rose-300"


bgClassLighter : Colour -> Attribute msg
bgClassLighter colour =
    Html.class <|
        case colour of
            Black ->
                "bg-gray-700"

            White ->
                "bg-white"

            MidGray ->
                "bg-gray-200"

            LightGray ->
                "bg-gray-100"

            LighterGray ->
                "bg-gray-50"

            LightestGray ->
                "bg-white"

            Gray ->
                "bg-gray-200"

            Red ->
                "bg-red-200"

            Orange ->
                "bg-orange-200"

            Amber ->
                "bg-amber-200"

            Yellow ->
                "bg-yellow-200"

            Lime ->
                "bg-lime-200"

            Green ->
                "bg-green-200"

            Emerald ->
                "bg-emerald-200"

            Teal ->
                "bg-teal-200"

            Cyan ->
                "bg-cyan-200"

            LightBlue ->
                "bg-lightBlue-200"

            Blue ->
                "bg-blue-200"

            Indigo ->
                "bg-indigo-200"

            Violet ->
                "bg-violet-200"

            Purple ->
                "bg-purple-200"

            Fuchsia ->
                "bg-fuchsia-200"

            Pink ->
                "bg-pink-200"

            Rose ->
                "bg-rose-200"


borderClassDarker : Colour -> Attribute msg
borderClassDarker colour =
    Html.class <|
        case colour of
            Black ->
                "border-gray-900"

            White ->
                "border-gray-50"

            MidGray ->
                "border-gray-400"

            LightGray ->
                "border-gray-300"

            LighterGray ->
                "border-gray-200"

            LightestGray ->
                "border-gray-100"

            Gray ->
                "border-gray-400"

            Red ->
                "border-red-400"

            Orange ->
                "border-orange-400"

            Amber ->
                "border-amber-400"

            Yellow ->
                "border-yellow-400"

            Lime ->
                "border-lime-400"

            Green ->
                "border-green-400"

            Emerald ->
                "border-emerald-400"

            Teal ->
                "border-teal-400"

            Cyan ->
                "border-cyan-400"

            LightBlue ->
                "border-lightBlue-400"

            Blue ->
                "border-blue-400"

            Indigo ->
                "border-indigo-400"

            Violet ->
                "border-violet-400"

            Purple ->
                "border-purple-400"

            Fuchsia ->
                "border-fuchsia-400"

            Pink ->
                "border-pink-400"

            Rose ->
                "border-rose-400"


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
