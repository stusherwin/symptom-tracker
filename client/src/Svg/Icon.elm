module Svg.Icon exposing (IconType(..), all, decode, encode, fillIcon, fromString, icon, iconSymbols, logo, toString, userSelectable)

import Html exposing (p)
import Json.Decode as D
import Json.Encode as E
import Svg exposing (Svg, defs, filter, g, linearGradient, node, path, stop, svg, symbol, use)
import Svg.Attributes exposing (class, d, dx, dy, gradientTransform, gradientUnits, id, offset, stdDeviation, style, transform, viewBox, x1, x2, xlinkHref, y1, y2)


logo : String -> Svg msg
logo logoClass =
    svg [ viewBox "0 0 24 24", class logoClass ]
        [ g []
            [ path
                [ transform "scale(0.03386667,0.03386667)"
                , d
                    "M 708.66211 147.7207 L 428.57227 434.375 L 234.28516 260.08984 L 0 503.83984 L 0 590.55078 C 0 655.98385 52.676304 708.66211 118.10938 708.66211 L 590.55078 708.66211 C 655.98385 708.66211 708.66211 655.98385 708.66211 590.55078 L 708.66211 147.7207 z "
                , style
                    "opacity:1;fill:#d8b4fe;fill-opacity:1;stroke:#1f2937;stroke-width:28.34645368;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1"
                ]
                []
            , path
                [ transform "scale(0.03386667,0.03386667)"
                , d
                    "M 118.10938 0 C 52.676304 0 0 52.676304 0 118.10938 L 0 503.83984 L 234.28516 260.08984 L 428.57227 434.375 L 708.66211 147.7207 L 708.66211 118.10938 C 708.66211 52.676304 655.98385 0 590.55078 0 L 118.10938 0 z "
                , style
                    "fill:#86efac;fill-opacity:1;fill-rule:evenodd;stroke:#1f2937;stroke-width:28.34645368;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;stroke-miterlimit:4;stroke-dasharray:none"
                ]
                []
            ]
        ]


icon : String -> IconType -> Svg msg
icon iconClass iconType =
    svg [ class "fill-current", class iconClass ]
        [ use [ xlinkHref ("#" ++ toString iconType) ] []
        ]


type IconType
    = SolidCheckCircle
    | SolidTimesCircle
    | SolidGrin
    | SolidFrownOpen
    | SolidMeh
    | SolidLaughBeam
    | SolidTired
    | SolidAngleDoubleLeft
    | SolidAngleDoubleRight
    | SolidAngleLeft
    | SolidAngleRight
    | SolidBars
    | SolidTimes
    | SolidChartLine
    | SolidCog
    | SolidCalendarDay
    | SolidCalendarAlt
    | SolidAngleDown
    | SolidAngleUp
    | SolidPlusCircle
    | SolidTrash
    | SolidTrashBan
    | SolidQuestionCircle
    | SolidEye
    | SolidEyeSlash
    | SolidArrowUp
    | SolidArrowDown
    | SolidCrosshairs
    | SolidPlus
    | SolidEquals
    | SolidPencilAlt
    | SolidTrashAlt
    | SolidExpand
    | SolidCompress
    | SolidCalendarMinus
    | SolidCalendarPlus
    | SolidMinus
    | SolidArrowRight
    | SolidCaretRight
    | SolidCalendarCheck
    | SolidChartArea


toString : IconType -> String
toString iconType =
    case iconType of
        SolidCheckCircle ->
            "solid-check-circle"

        SolidTimesCircle ->
            "solid-times-circle"

        SolidGrin ->
            "solid-grin"

        SolidFrownOpen ->
            "solid-frown-open"

        SolidMeh ->
            "solid-meh"

        SolidLaughBeam ->
            "solid-laugh-beam"

        SolidTired ->
            "solid-tired"

        SolidAngleDoubleLeft ->
            "solid-angle-double-left"

        SolidAngleDoubleRight ->
            "solid-angle-double-right"

        SolidAngleLeft ->
            "solid-angle-left"

        SolidAngleRight ->
            "solid-angle-right"

        SolidBars ->
            "solid-bars"

        SolidTimes ->
            "solid-times"

        SolidChartLine ->
            "solid-chart-line"

        SolidCog ->
            "solid-cog"

        SolidCalendarDay ->
            "solid-calendar-day"

        SolidCalendarAlt ->
            "solid-calendar-alt"

        SolidAngleDown ->
            "solid-angle-down"

        SolidAngleUp ->
            "solid-angle-up"

        SolidPlusCircle ->
            "solid-plus-circle"

        SolidTrash ->
            "solid-trash"

        SolidTrashBan ->
            "solid-trash-ban"

        SolidQuestionCircle ->
            "solid-question-circle"

        SolidEye ->
            "solid-eye"

        SolidEyeSlash ->
            "solid-eye-slash"

        SolidArrowUp ->
            "solid-arrow-up"

        SolidArrowDown ->
            "solid-arrow-down"

        SolidCrosshairs ->
            "solid-crosshairs"

        SolidPlus ->
            "solid-plus"

        SolidEquals ->
            "solid-equals"

        SolidPencilAlt ->
            "solid-pencil-alt"

        SolidTrashAlt ->
            "solid-trash-alt"

        SolidExpand ->
            "solid-expand"

        SolidCompress ->
            "solid-compress"

        SolidCalendarMinus ->
            "solid-calendar-minus"

        SolidCalendarPlus ->
            "solid-calendar-plus"

        SolidMinus ->
            "solid-minus"

        SolidArrowRight ->
            "solid-arrow-right"

        SolidCaretRight ->
            "solid-caret-right"

        SolidCalendarCheck ->
            "solid-calendar-check"
            
        SolidChartArea -> 
            "solid-chart-area"


fromString : String -> Maybe IconType
fromString str =
    case str of
        "solid-check-circle" ->
            Just SolidCheckCircle

        "solid-times-circle" ->
            Just SolidTimesCircle

        "solid-grin" ->
            Just SolidGrin

        "solid-frown-open" ->
            Just SolidFrownOpen

        "solid-meh" ->
            Just SolidMeh

        "solid-laugh-beam" ->
            Just SolidLaughBeam

        "solid-tired" ->
            Just SolidTired

        "solid-angle-double-left" ->
            Just SolidAngleDoubleLeft

        "solid-angle-double-right" ->
            Just SolidAngleDoubleRight

        "solid-angle-left" ->
            Just SolidAngleLeft

        "solid-angle-right" ->
            Just SolidAngleRight

        "solid-bars" ->
            Just SolidBars

        "solid-times" ->
            Just SolidTimes

        "solid-chart-line" ->
            Just SolidChartLine

        "solid-cog" ->
            Just SolidCog

        "solid-calendar-day" ->
            Just SolidCalendarDay

        "solid-calendar-alt" ->
            Just SolidCalendarAlt

        "solid-angle-down" ->
            Just SolidAngleDown

        "solid-angle-up" ->
            Just SolidAngleUp

        "solid-plus-circle" ->
            Just SolidPlusCircle

        "solid-trash" ->
            Just SolidTrash

        "solid-trash-ban" ->
            Just SolidTrashBan

        "solid-question-circle" ->
            Just SolidQuestionCircle

        "solid-eye" ->
            Just SolidEye

        "solid-eye-slash" ->
            Just SolidEyeSlash

        "solid-arrow-up" ->
            Just SolidArrowUp

        "solid-arrow-down" ->
            Just SolidArrowDown

        "solid-crosshairs" ->
            Just SolidCrosshairs

        "solid-plus" ->
            Just SolidPlus

        "solid-equals" ->
            Just SolidEquals

        "solid-pencil-alt" ->
            Just SolidPencilAlt

        "solid-trash-alt" ->
            Just SolidTrashAlt

        "solid-expand" ->
            Just SolidExpand

        "solid-compress" ->
            Just SolidCompress

        "solid-calendar-minus" ->
            Just SolidCalendarMinus

        "solid-calendar-plus" ->
            Just SolidCalendarPlus

        "solid-minus" ->
            Just SolidMinus

        "solid-arrow-right" ->
            Just SolidArrowRight

        "solid-caret-right" ->
            Just SolidCaretRight

        "solid-calendar-check" ->
            Just SolidCalendarCheck
            
        "solid-chart-area" ->
            Just SolidChartArea 

        _ ->
            Nothing


iconSymbol : List (Svg.Attribute msg) -> IconType -> Svg msg
iconSymbol attrs iconType =
    case iconType of
        SolidCheckCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M504 256c0 136.967-111.033 248-248 248S8 392.967 8 256 119.033 8 256 8s248 111.033 248 248zM227.314 387.314l184-184c6.248-6.248 6.248-16.379 0-22.627l-22.627-22.627c-6.248-6.249-16.379-6.249-22.628 0L216 308.118l-70.059-70.059c-6.248-6.248-16.379-6.248-22.628 0l-22.627 22.627c-6.248 6.248-6.248 16.379 0 22.627l104 104c6.249 6.249 16.379 6.249 22.628.001z" ] ++ attrs) []
                ]

        SolidTimesCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm121.6 313.1c4.7 4.7 4.7 12.3 0 17L338 377.6c-4.7 4.7-12.3 4.7-17 0L256 312l-65.1 65.6c-4.7 4.7-12.3 4.7-17 0L134.4 338c-4.7-4.7-4.7-12.3 0-17l65.6-65-65.6-65.1c-4.7-4.7-4.7-12.3 0-17l39.6-39.6c4.7-4.7 12.3-4.7 17 0l65 65.7 65.1-65.6c4.7-4.7 12.3-4.7 17 0l39.6 39.6c4.7 4.7 4.7 12.3 0 17L312 256l65.6 65.1z" ] ++ attrs) []
                ]

        SolidGrin ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path ([ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm80 168c17.7 0 32 14.3 32 32s-14.3 32-32 32-32-14.3-32-32 14.3-32 32-32zm-160 0c17.7 0 32 14.3 32 32s-14.3 32-32 32-32-14.3-32-32 14.3-32 32-32zm80 256c-60.6 0-134.5-38.3-143.8-93.3-2-11.8 9.3-21.6 20.7-17.9C155.1 330.5 200 336 248 336s92.9-5.5 123.1-15.2c11.3-3.7 22.6 6.1 20.7 17.9-9.3 55-83.2 93.3-143.8 93.3z" ] ++ attrs) []
                ]

        SolidFrownOpen ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path ([ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zM136 208c0-17.7 14.3-32 32-32s32 14.3 32 32-14.3 32-32 32-32-14.3-32-32zm187.3 183.3c-31.2-9.6-59.4-15.3-75.3-15.3s-44.1 5.7-75.3 15.3c-11.5 3.5-22.5-6.3-20.5-18.1 7-40 60.1-61.2 95.8-61.2s88.8 21.3 95.8 61.2c2 11.9-9.1 21.6-20.5 18.1zM328 240c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32z" ] ++ attrs) []
                ]

        SolidMeh ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path ([ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm-80 168c17.7 0 32 14.3 32 32s-14.3 32-32 32-32-14.3-32-32 14.3-32 32-32zm176 192H152c-21.2 0-21.2-32 0-32h192c21.2 0 21.2 32 0 32zm-16-128c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32z" ] ++ attrs) []
                ]

        SolidLaughBeam ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path ([ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm24 199.4c3.3-42.1 32.2-71.4 56-71.4s52.7 29.3 56 71.4c.7 8.6-10.8 11.9-14.9 4.5l-9.5-17c-7.7-13.7-19.2-21.6-31.5-21.6s-23.8 7.9-31.5 21.6l-9.5 17c-4.2 7.4-15.8 4.1-15.1-4.5zm-160 0c3.3-42.1 32.2-71.4 56-71.4s52.7 29.3 56 71.4c.7 8.6-10.8 11.9-14.9 4.5l-9.5-17c-7.7-13.7-19.2-21.6-31.5-21.6s-23.8 7.9-31.5 21.6l-9.5 17c-4.3 7.4-15.8 4-15.1-4.5zM398.9 306C390 377 329.4 432 256 432h-16c-73.4 0-134-55-142.9-126-1.2-9.5 6.3-18 15.9-18h270c9.6 0 17.1 8.4 15.9 18z" ] ++ attrs) []
                ]

        SolidTired ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path ([ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm33.8 189.7l80-48c11.6-6.9 24 7.7 15.4 18L343.6 208l33.6 40.3c8.7 10.4-3.9 24.8-15.4 18l-80-48c-7.7-4.7-7.7-15.9 0-20.6zm-163-30c-8.6-10.3 3.8-24.9 15.4-18l80 48c7.8 4.7 7.8 15.9 0 20.6l-80 48c-11.5 6.8-24-7.6-15.4-18l33.6-40.3-33.6-40.3zM248 288c51.9 0 115.3 43.8 123.2 106.7 1.7 13.6-8 24.6-17.7 20.4-25.9-11.1-64.4-17.4-105.5-17.4s-79.6 6.3-105.5 17.4c-9.8 4.2-19.4-7-17.7-20.4C132.7 331.8 196.1 288 248 288z" ] ++ attrs) []
                ]

        SolidAngleDoubleLeft ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M223.7 239l136-136c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9L319.9 256l96.4 96.4c9.4 9.4 9.4 24.6 0 33.9L393.7 409c-9.4 9.4-24.6 9.4-33.9 0l-136-136c-9.5-9.4-9.5-24.6-.1-34zm-192 34l136 136c9.4 9.4 24.6 9.4 33.9 0l22.6-22.6c9.4-9.4 9.4-24.6 0-33.9L127.9 256l96.4-96.4c9.4-9.4 9.4-24.6 0-33.9L201.7 103c-9.4-9.4-24.6-9.4-33.9 0l-136 136c-9.5 9.4-9.5 24.6-.1 34z" ] ++ attrs) []
                ]

        SolidAngleDoubleRight ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M224.3 273l-136 136c-9.4 9.4-24.6 9.4-33.9 0l-22.6-22.6c-9.4-9.4-9.4-24.6 0-33.9l96.4-96.4-96.4-96.4c-9.4-9.4-9.4-24.6 0-33.9L54.3 103c9.4-9.4 24.6-9.4 33.9 0l136 136c9.5 9.4 9.5 24.6.1 34zm192-34l-136-136c-9.4-9.4-24.6-9.4-33.9 0l-22.6 22.6c-9.4 9.4-9.4 24.6 0 33.9l96.4 96.4-96.4 96.4c-9.4 9.4-9.4 24.6 0 33.9l22.6 22.6c9.4 9.4 24.6 9.4 33.9 0l136-136c9.4-9.2 9.4-24.4 0-33.8z" ] ++ attrs) []
                ]

        SolidAngleLeft ->
            symbol [ id <| toString iconType, viewBox "0 0 256 512" ]
                [ path ([ d "M31.7 239l136-136c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9L127.9 256l96.4 96.4c9.4 9.4 9.4 24.6 0 33.9L201.7 409c-9.4 9.4-24.6 9.4-33.9 0l-136-136c-9.5-9.4-9.5-24.6-.1-34z" ] ++ attrs) []
                ]

        SolidAngleRight ->
            symbol [ id <| toString iconType, viewBox "0 0 256 512" ]
                [ path ([ d "M224.3 273l-136 136c-9.4 9.4-24.6 9.4-33.9 0l-22.6-22.6c-9.4-9.4-9.4-24.6 0-33.9l96.4-96.4-96.4-96.4c-9.4-9.4-9.4-24.6 0-33.9L54.3 103c9.4-9.4 24.6-9.4 33.9 0l136 136c9.5 9.4 9.5 24.6.1 34z" ] ++ attrs) []
                ]

        SolidBars ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M16 132h416c8.837 0 16-7.163 16-16V76c0-8.837-7.163-16-16-16H16C7.163 60 0 67.163 0 76v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16z" ] ++ attrs) []
                ]

        SolidTimes ->
            symbol [ id <| toString iconType, viewBox "0 0 352 512" ]
                [ path ([ d "M242.72 256l100.07-100.07c12.28-12.28 12.28-32.19 0-44.48l-22.24-22.24c-12.28-12.28-32.19-12.28-44.48 0L176 189.28 75.93 89.21c-12.28-12.28-32.19-12.28-44.48 0L9.21 111.45c-12.28 12.28-12.28 32.19 0 44.48L109.28 256 9.21 356.07c-12.28 12.28-12.28 32.19 0 44.48l22.24 22.24c12.28 12.28 32.2 12.28 44.48 0L176 322.72l100.07 100.07c12.28 12.28 32.2 12.28 44.48 0l22.24-22.24c12.28-12.28 12.28-32.19 0-44.48L242.72 256z" ] ++ attrs) []
                ]

        SolidChartLine ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M496 384H64V80c0-8.84-7.16-16-16-16H16C7.16 64 0 71.16 0 80v336c0 17.67 14.33 32 32 32h464c8.84 0 16-7.16 16-16v-32c0-8.84-7.16-16-16-16zM464 96H345.94c-21.38 0-32.09 25.85-16.97 40.97l32.4 32.4L288 242.75l-73.37-73.37c-12.5-12.5-32.76-12.5-45.25 0l-68.69 68.69c-6.25 6.25-6.25 16.38 0 22.63l22.62 22.62c6.25 6.25 16.38 6.25 22.63 0L192 237.25l73.37 73.37c12.5 12.5 32.76 12.5 45.25 0l96-96 32.4 32.4c15.12 15.12 40.97 4.41 40.97-16.97V112c.01-8.84-7.15-16-15.99-16z" ] ++ attrs) []
                ]

        SolidCog ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M487.4 315.7l-42.6-24.6c4.3-23.2 4.3-47 0-70.2l42.6-24.6c4.9-2.8 7.1-8.6 5.5-14-11.1-35.6-30-67.8-54.7-94.6-3.8-4.1-10-5.1-14.8-2.3L380.8 110c-17.9-15.4-38.5-27.3-60.8-35.1V25.8c0-5.6-3.9-10.5-9.4-11.7-36.7-8.2-74.3-7.8-109.2 0-5.5 1.2-9.4 6.1-9.4 11.7V75c-22.2 7.9-42.8 19.8-60.8 35.1L88.7 85.5c-4.9-2.8-11-1.9-14.8 2.3-24.7 26.7-43.6 58.9-54.7 94.6-1.7 5.4.6 11.2 5.5 14L67.3 221c-4.3 23.2-4.3 47 0 70.2l-42.6 24.6c-4.9 2.8-7.1 8.6-5.5 14 11.1 35.6 30 67.8 54.7 94.6 3.8 4.1 10 5.1 14.8 2.3l42.6-24.6c17.9 15.4 38.5 27.3 60.8 35.1v49.2c0 5.6 3.9 10.5 9.4 11.7 36.7 8.2 74.3 7.8 109.2 0 5.5-1.2 9.4-6.1 9.4-11.7v-49.2c22.2-7.9 42.8-19.8 60.8-35.1l42.6 24.6c4.9 2.8 11 1.9 14.8-2.3 24.7-26.7 43.6-58.9 54.7-94.6 1.5-5.5-.7-11.3-5.6-14.1zM256 336c-44.1 0-80-35.9-80-80s35.9-80 80-80 80 35.9 80 80-35.9 80-80 80z" ] ++ attrs) []
                ]

        SolidCalendarDay ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M0 464c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V192H0v272zm64-192c0-8.8 7.2-16 16-16h96c8.8 0 16 7.2 16 16v96c0 8.8-7.2 16-16 16H80c-8.8 0-16-7.2-16-16v-96zM400 64h-48V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H160V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H48C21.5 64 0 85.5 0 112v48h448v-48c0-26.5-21.5-48-48-48z" ] ++ attrs) []
                ]

        SolidCalendarAlt ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M0 464c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V192H0v272zm320-196c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zm0 128c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zM192 268c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zm0 128c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zM64 268c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H76c-6.6 0-12-5.4-12-12v-40zm0 128c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H76c-6.6 0-12-5.4-12-12v-40zM400 64h-48V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H160V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H48C21.5 64 0 85.5 0 112v48h448v-48c0-26.5-21.5-48-48-48z" ] ++ attrs) []
                ]

        SolidAngleDown ->
            symbol [ id <| toString iconType, viewBox "0 0 320 512" ]
                [ path ([ d "M143 352.3L7 216.3c-9.4-9.4-9.4-24.6 0-33.9l22.6-22.6c9.4-9.4 24.6-9.4 33.9 0l96.4 96.4 96.4-96.4c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9l-136 136c-9.2 9.4-24.4 9.4-33.8 0z" ] ++ attrs) []
                ]

        SolidAngleUp ->
            symbol [ id <| toString iconType, viewBox "0 0 320 512" ]
                [ path ([ d "M177 159.7l136 136c9.4 9.4 9.4 24.6 0 33.9l-22.6 22.6c-9.4 9.4-24.6 9.4-33.9 0L160 255.9l-96.4 96.4c-9.4 9.4-24.6 9.4-33.9 0L7 329.7c-9.4-9.4-9.4-24.6 0-33.9l136-136c9.4-9.5 24.6-9.5 34-.1z" ] ++ attrs) []
                ]

        SolidPlusCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm144 276c0 6.6-5.4 12-12 12h-92v92c0 6.6-5.4 12-12 12h-56c-6.6 0-12-5.4-12-12v-92h-92c-6.6 0-12-5.4-12-12v-56c0-6.6 5.4-12 12-12h92v-92c0-6.6 5.4-12 12-12h56c6.6 0 12 5.4 12 12v92h92c6.6 0 12 5.4 12 12v56z" ] ++ attrs) []
                ]

        SolidTrash ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M432 32H312l-9.4-18.7A24 24 0 0 0 281.1 0H166.8a23.72 23.72 0 0 0-21.4 13.3L136 32H16A16 16 0 0 0 0 48v32a16 16 0 0 0 16 16h416a16 16 0 0 0 16-16V48a16 16 0 0 0-16-16zM53.2 467a48 48 0 0 0 47.9 45h245.8a48 48 0 0 0 47.9-45L416 128H32z" ] ++ attrs) []
                ]

        SolidTrashBan ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M 198.8 0.00035 C 189.706 -0.0394213 181.391 5.12832 177.4 13.3004 L 168 32.0004 L 48.0001 32.0004 C 43.7566 32.0004 39.687 33.6861 36.6864 36.6867 C 33.6858 39.6873 32.0001 43.757 32.0001 48.0004 L 32.0001 80.0005 C 32.0001 84.244 33.6858 88.3136 36.6864 91.3142 C 39.687 94.3148 43.7566 96.0005 48.0001 96.0005 L 464.001 96.0005 C 468.244 96.0005 472.314 94.3148 475.315 91.3142 C 478.315 88.3136 480.001 84.244 480.001 80.0005 L 480.001 48.0004 C 480.001 43.757 478.315 39.6873 475.315 36.6867 C 472.314 33.6861 468.244 32.0004 464.001 32.0004 L 344.001 32.0004 L 334.601 13.3004 C 330.539 5.14579 322.211 -0.00618451 313.101 0.00035 L 198.8 0.00035 Z M 64.0001 128.001 L 85.2002 467.001 C 86.7841 492.294 107.758 511.998 133.1 512.001 L 378.901 512.001 C 404.243 511.998 425.217 492.294 426.801 467.001 L 448.001 128.001 L 64.0001 128.001 Z M 256 169.472 C 339.338 169.472 406.858 233.345 406.858 312.182 C 406.858 391.019 339.338 454.896 256 454.896 C 172.663 454.896 105.143 391.019 105.143 312.182 C 105.143 233.345 172.663 169.472 256 169.472 Z M 255 206.286 C 232.659 206.488 210.507 212.969 191.824 225.406 L 347.734 372.892 C 377.723 332.552 374.922 274.95 335.139 237.315 C 312.761 216.146 283.725 206.028 255 206.286 Z M 164.267 251.472 C 134.278 291.812 137.079 349.414 176.862 387.049 C 216.645 424.683 277.474 427.386 320.177 398.958 L 164.267 251.472 Z" ] ++ attrs) []
                ]

        SolidQuestionCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M504 256c0 136.997-111.043 248-248 248S8 392.997 8 256C8 119.083 119.043 8 256 8s248 111.083 248 248zM262.655 90c-54.497 0-89.255 22.957-116.549 63.758-3.536 5.286-2.353 12.415 2.715 16.258l34.699 26.31c5.205 3.947 12.621 3.008 16.665-2.122 17.864-22.658 30.113-35.797 57.303-35.797 20.429 0 45.698 13.148 45.698 32.958 0 14.976-12.363 22.667-32.534 33.976C247.128 238.528 216 254.941 216 296v4c0 6.627 5.373 12 12 12h56c6.627 0 12-5.373 12-12v-1.333c0-28.462 83.186-29.647 83.186-106.667 0-58.002-60.165-102-116.531-102zM256 338c-25.365 0-46 20.635-46 46 0 25.364 20.635 46 46 46s46-20.636 46-46c0-25.365-20.635-46-46-46z" ] ++ attrs) []
                ]

        SolidEye ->
            symbol [ id <| toString iconType, viewBox "0 0 576 512" ]
                [ path ([ d "M572.52 241.4C518.29 135.59 410.93 64 288 64S57.68 135.64 3.48 241.41a32.35 32.35 0 0 0 0 29.19C57.71 376.41 165.07 448 288 448s230.32-71.64 284.52-177.41a32.35 32.35 0 0 0 0-29.19zM288 400a144 144 0 1 1 144-144 143.93 143.93 0 0 1-144 144zm0-240a95.31 95.31 0 0 0-25.31 3.79 47.85 47.85 0 0 1-66.9 66.9A95.78 95.78 0 1 0 288 160z" ] ++ attrs) []
                ]

        SolidEyeSlash ->
            symbol [ id <| toString iconType, viewBox "0 0 640 512" ]
                [ path ([ d "M320 400c-75.85 0-137.25-58.71-142.9-133.11L72.2 185.82c-13.79 17.3-26.48 35.59-36.72 55.59a32.35 32.35 0 0 0 0 29.19C89.71 376.41 197.07 448 320 448c26.91 0 52.87-4 77.89-10.46L346 397.39a144.13 144.13 0 0 1-26 2.61zm313.82 58.1l-110.55-85.44a331.25 331.25 0 0 0 81.25-102.07 32.35 32.35 0 0 0 0-29.19C550.29 135.59 442.93 64 320 64a308.15 308.15 0 0 0-147.32 37.7L45.46 3.37A16 16 0 0 0 23 6.18L3.37 31.45A16 16 0 0 0 6.18 53.9l588.36 454.73a16 16 0 0 0 22.46-2.81l19.64-25.27a16 16 0 0 0-2.82-22.45zm-183.72-142l-39.3-30.38A94.75 94.75 0 0 0 416 256a94.76 94.76 0 0 0-121.31-92.21A47.65 47.65 0 0 1 304 192a46.64 46.64 0 0 1-1.54 10l-73.61-56.89A142.31 142.31 0 0 1 320 112a143.92 143.92 0 0 1 144 144c0 21.63-5.29 41.79-13.9 60.11z" ] ++ attrs) []
                ]

        SolidArrowDown ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M413.1 222.5l22.2 22.2c9.4 9.4 9.4 24.6 0 33.9L241 473c-9.4 9.4-24.6 9.4-33.9 0L12.7 278.6c-9.4-9.4-9.4-24.6 0-33.9l22.2-22.2c9.5-9.5 25-9.3 34.3.4L184 343.4V56c0-13.3 10.7-24 24-24h32c13.3 0 24 10.7 24 24v287.4l114.8-120.5c9.3-9.8 24.8-10 34.3-.4z" ] ++ attrs) []
                ]

        SolidArrowUp ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M34.9 289.5l-22.2-22.2c-9.4-9.4-9.4-24.6 0-33.9L207 39c9.4-9.4 24.6-9.4 33.9 0l194.3 194.3c9.4 9.4 9.4 24.6 0 33.9L413 289.4c-9.5 9.5-25 9.3-34.3-.4L264 168.6V456c0 13.3-10.7 24-24 24h-32c-13.3 0-24-10.7-24-24V168.6L69.2 289.1c-9.3 9.8-24.8 10-34.3.4z" ] ++ attrs) []
                ]

        SolidCrosshairs ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M500 224h-30.364C455.724 130.325 381.675 56.276 288 42.364V12c0-6.627-5.373-12-12-12h-40c-6.627 0-12 5.373-12 12v30.364C130.325 56.276 56.276 130.325 42.364 224H12c-6.627 0-12 5.373-12 12v40c0 6.627 5.373 12 12 12h30.364C56.276 381.675 130.325 455.724 224 469.636V500c0 6.627 5.373 12 12 12h40c6.627 0 12-5.373 12-12v-30.364C381.675 455.724 455.724 381.675 469.636 288H500c6.627 0 12-5.373 12-12v-40c0-6.627-5.373-12-12-12zM288 404.634V364c0-6.627-5.373-12-12-12h-40c-6.627 0-12 5.373-12 12v40.634C165.826 392.232 119.783 346.243 107.366 288H148c6.627 0 12-5.373 12-12v-40c0-6.627-5.373-12-12-12h-40.634C119.768 165.826 165.757 119.783 224 107.366V148c0 6.627 5.373 12 12 12h40c6.627 0 12-5.373 12-12v-40.634C346.174 119.768 392.217 165.757 404.634 224H364c-6.627 0-12 5.373-12 12v40c0 6.627 5.373 12 12 12h40.634C392.232 346.174 346.243 392.217 288 404.634zM288 256c0 17.673-14.327 32-32 32s-32-14.327-32-32c0-17.673 14.327-32 32-32s32 14.327 32 32z" ] ++ attrs) []
                ]

        SolidPlus ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M416 208H272V64c0-17.67-14.33-32-32-32h-32c-17.67 0-32 14.33-32 32v144H32c-17.67 0-32 14.33-32 32v32c0 17.67 14.33 32 32 32h144v144c0 17.67 14.33 32 32 32h32c17.67 0 32-14.33 32-32V304h144c17.67 0 32-14.33 32-32v-32c0-17.67-14.33-32-32-32z" ] ++ attrs) []
                ]

        SolidEquals ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M416 304H32c-17.67 0-32 14.33-32 32v32c0 17.67 14.33 32 32 32h384c17.67 0 32-14.33 32-32v-32c0-17.67-14.33-32-32-32zm0-192H32c-17.67 0-32 14.33-32 32v32c0 17.67 14.33 32 32 32h384c17.67 0 32-14.33 32-32v-32c0-17.67-14.33-32-32-32z" ] ++ attrs) []
                ]

        SolidPencilAlt ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path ([ d "M497.9 142.1l-46.1 46.1c-4.7 4.7-12.3 4.7-17 0l-111-111c-4.7-4.7-4.7-12.3 0-17l46.1-46.1c18.7-18.7 49.1-18.7 67.9 0l60.1 60.1c18.8 18.7 18.8 49.1 0 67.9zM284.2 99.8L21.6 362.4.4 483.9c-2.9 16.4 11.4 30.6 27.8 27.8l121.5-21.3 262.6-262.6c4.7-4.7 4.7-12.3 0-17l-111-111c-4.8-4.7-12.4-4.7-17.1 0zM124.1 339.9c-5.5-5.5-5.5-14.3 0-19.8l154-154c5.5-5.5 14.3-5.5 19.8 0s5.5 14.3 0 19.8l-154 154c-5.5 5.5-14.3 5.5-19.8 0zM88 424h48v36.3l-64.5 11.3-31.1-31.1L51.7 376H88v48z" ] ++ attrs) []
                ]

        SolidTrashAlt ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M32 464a48 48 0 0 0 48 48h288a48 48 0 0 0 48-48V128H32zm272-256a16 16 0 0 1 32 0v224a16 16 0 0 1-32 0zm-96 0a16 16 0 0 1 32 0v224a16 16 0 0 1-32 0zm-96 0a16 16 0 0 1 32 0v224a16 16 0 0 1-32 0zM432 32H312l-9.4-18.7A24 24 0 0 0 281.1 0H166.8a23.72 23.72 0 0 0-21.4 13.3L136 32H16A16 16 0 0 0 0 48v32a16 16 0 0 0 16 16h416a16 16 0 0 0 16-16V48a16 16 0 0 0-16-16z" ] ++ attrs) []
                ]

        SolidExpand ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M0 180V56c0-13.3 10.7-24 24-24h124c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H64v84c0 6.6-5.4 12-12 12H12c-6.6 0-12-5.4-12-12zM288 44v40c0 6.6 5.4 12 12 12h84v84c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12V56c0-13.3-10.7-24-24-24H300c-6.6 0-12 5.4-12 12zm148 276h-40c-6.6 0-12 5.4-12 12v84h-84c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h124c13.3 0 24-10.7 24-24V332c0-6.6-5.4-12-12-12zM160 468v-40c0-6.6-5.4-12-12-12H64v-84c0-6.6-5.4-12-12-12H12c-6.6 0-12 5.4-12 12v124c0 13.3 10.7 24 24 24h124c6.6 0 12-5.4 12-12z" ] ++ attrs) []
                ]

        SolidCompress ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M436 192H312c-13.3 0-24-10.7-24-24V44c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v84h84c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12zm-276-24V44c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v84H12c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h124c13.3 0 24-10.7 24-24zm0 300V344c0-13.3-10.7-24-24-24H12c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h84v84c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm192 0v-84h84c6.6 0 12-5.4 12-12v-40c0-6.6-5.4-12-12-12H312c-13.3 0-24 10.7-24 24v124c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12z" ] ++ attrs) []
                ]

        SolidCalendarMinus ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M436 160H12c-6.6 0-12-5.4-12-12v-36c0-26.5 21.5-48 48-48h48V12c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v52h128V12c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v52h48c26.5 0 48 21.5 48 48v36c0 6.6-5.4 12-12 12zM12 192h424c6.6 0 12 5.4 12 12v260c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V204c0-6.6 5.4-12 12-12zm304 192c6.6 0 12-5.4 12-12v-40c0-6.6-5.4-12-12-12H132c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h184z" ] ++ attrs) []
                ]

        SolidCalendarPlus ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M436 160H12c-6.6 0-12-5.4-12-12v-36c0-26.5 21.5-48 48-48h48V12c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v52h128V12c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v52h48c26.5 0 48 21.5 48 48v36c0 6.6-5.4 12-12 12zM12 192h424c6.6 0 12 5.4 12 12v260c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V204c0-6.6 5.4-12 12-12zm316 140c0-6.6-5.4-12-12-12h-60v-60c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v60h-60c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h60v60c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12v-60h60c6.6 0 12-5.4 12-12v-40z" ] ++ attrs) []
                ]

        SolidMinus ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M416 208H32c-17.67 0-32 14.33-32 32v32c0 17.67 14.33 32 32 32h384c17.67 0 32-14.33 32-32v-32c0-17.67-14.33-32-32-32z" ] ++ attrs) []
                ]

        SolidArrowRight ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path ([ d "M190.5 66.9l22.2-22.2c9.4-9.4 24.6-9.4 33.9 0L441 239c9.4 9.4 9.4 24.6 0 33.9L246.6 467.3c-9.4 9.4-24.6 9.4-33.9 0l-22.2-22.2c-9.5-9.5-9.3-25 .4-34.3L311.4 296H24c-13.3 0-24-10.7-24-24v-32c0-13.3 10.7-24 24-24h287.4L190.9 101.2c-9.8-9.3-10-24.8-.4-34.3z" ] ++ attrs) []
                ]

        SolidCaretRight ->
            symbol [ id <| toString iconType, viewBox "0 0 192 512" ]
                [ path ([ d "M0 384.662V127.338c0-17.818 21.543-26.741 34.142-14.142l128.662 128.662c7.81 7.81 7.81 20.474 0 28.284L34.142 398.804C21.543 411.404 0 402.48 0 384.662z" ] ++ attrs) []
                ]

        SolidCalendarCheck ->
            symbol [id <| toString iconType, viewBox "0 0 448 512"] [
              path ([d "M436 160H12c-6.627 0-12-5.373-12-12v-36c0-26.51 21.49-48 48-48h48V12c0-6.627 5.373-12 12-12h40c6.627 0 12 5.373 12 12v52h128V12c0-6.627 5.373-12 12-12h40c6.627 0 12 5.373 12 12v52h48c26.51 0 48 21.49 48 48v36c0 6.627-5.373 12-12 12zM12 192h424c6.627 0 12 5.373 12 12v260c0 26.51-21.49 48-48 48H48c-26.51 0-48-21.49-48-48V204c0-6.627 5.373-12 12-12zm333.296 95.947l-28.169-28.398c-4.667-4.705-12.265-4.736-16.97-.068L194.12 364.665l-45.98-46.352c-4.667-4.705-12.266-4.736-16.971-.068l-28.397 28.17c-4.705 4.667-4.736 12.265-.068 16.97l82.601 83.269c4.667 4.705 12.265 4.736 16.97.068l142.953-141.805c4.705-4.667 4.736-12.265.068-16.97z"] ++ attrs) []
            ]

        SolidChartArea ->
            symbol [id <| toString iconType, viewBox "0 0 512 512"] [
              path ([d "M500 384c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H12c-6.6 0-12-5.4-12-12V76c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v308h436zM372.7 159.5L288 216l-85.3-113.7c-5.1-6.8-15.5-6.3-19.9 1L96 248v104h384l-89.9-187.8c-3.2-6.5-11.4-8.7-17.4-4.7z"]  ++ attrs) []
            ]


all : List IconType
all =
    [ SolidCheckCircle
    , SolidTimesCircle
    , SolidGrin
    , SolidFrownOpen
    , SolidMeh
    , SolidLaughBeam
    , SolidTired
    , SolidAngleDoubleLeft
    , SolidAngleDoubleRight
    , SolidAngleLeft
    , SolidAngleRight
    , SolidBars
    , SolidTimes
    , SolidChartLine
    , SolidCog
    , SolidCalendarDay
    , SolidCalendarAlt
    , SolidAngleDown
    , SolidAngleUp
    , SolidPlusCircle
    , SolidTrash
    , SolidTrashBan
    , SolidQuestionCircle
    , SolidEye
    , SolidEyeSlash
    , SolidArrowUp
    , SolidArrowDown
    , SolidCrosshairs
    , SolidPlus
    , SolidEquals
    , SolidPencilAlt
    , SolidTrashAlt
    , SolidExpand
    , SolidCompress
    , SolidCalendarMinus
    , SolidCalendarPlus
    , SolidMinus
    , SolidArrowRight
    , SolidCaretRight
    , SolidCalendarCheck
    , SolidChartArea
    ]


userSelectable : List IconType
userSelectable =
    [ SolidGrin
    , SolidFrownOpen
    , SolidMeh
    , SolidLaughBeam
    , SolidTired
    , SolidQuestionCircle
    ]


iconSymbols : Svg msg
iconSymbols =
    svg [ style "width: 0; height: 0;" ] <|
        [ defs []
            [-- filter [ id "shadow" ] [
             --     node "feDropShadow" [dx "0.2", dy "0.4", stdDeviation "0.2" ] []
             -- ]
            ]
        ]
            ++ List.map (iconSymbol [{- style "filter: url(#shadow);" -}]) all


decode : D.Decoder IconType
decode =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just iconType ->
                        D.succeed iconType

                    _ ->
                        D.fail <| "Invalid IconType: " ++ str
            )


encode : IconType -> E.Value
encode =
    E.string << toString


fillIcon : String -> Bool -> Svg msg
fillIcon iconClass filled =
    svg
        [ viewBox "0 0 448 512", class iconClass ]
    <|
        defs []
            [ linearGradient
                [ id "linearGradient1432" ]
                [ stop
                    [ style "stop-color:#9ca3af;stop-opacity:1"
                    , offset "0"
                    , id "stop1428"
                    ]
                    []
                , stop
                    [ style "stop-color:#9ca3af;stop-opacity:0.09558824"
                    , offset "1"
                    , id "stop1430"
                    ]
                    []
                ]
            , linearGradient
                [ xlinkHref "#linearGradient1432"
                , id "linearGradient1434"
                , x1 "159.03133"
                , y1 "100.96701"
                , x2 "160.79675"
                , y2 "288.48538"
                , gradientUnits "userSpaceOnUse"
                , gradientTransform "matrix(1.7716536,0,0,1.7716536,0,-42.283507)"
                ]
                []
            ]
            -- v4
            :: path
                [ id "backstroke2" -- fill:#000000;fill-opacity:1;
                , style "fill:#000000; stroke: none"
                , class "stroke-back"
                , d "M 261.806 144.778 C 261.055 145.758 260.301 146.754 259.536 147.781 C 247.638 163.772 235.858 184.795 223.747 208.425 C 232.943 226.238 242.32 245.623 252.083 265.542 C 274.137 310.543 298.382 358.421 331.874 398.429 C 361.968 434.377 401.113 464.196 450.576 475.113 L 453.168 475.113 L 453.168 409.037 C 424.564 400.28 401.633 382.066 380.089 356.331 C 353.228 324.243 330.475 280.761 308.806 236.546 C 293.405 205.121 278.666 173.422 261.806 144.778 Z"
                ]
                []
            :: (if filled then
                    [ path
                        [ id "backfill" --fill:#000000;fill-opacity:0.15;
                        , style "fill:#000000; stroke: none"
                        , class "fill-back"
                        , d "M 122.205 114.066 C 101.645 114.01 86.8659 124.968 67.6881 150.742 C 47.2318 178.234 27.1353 220.542 5.0811 265.542 C 3.40309 268.966 1.70648 272.411 0 275.863 L 0 478.074 L 453.168 478.074 L 453.168 475.665 C 402.478 465.232 362.491 435.003 331.874 398.429 C 298.382 358.421 274.137 310.543 252.083 265.542 C 230.028 220.542 209.933 178.234 189.477 150.742 C 169.02 123.249 153.568 112.615 130.767 114.223 L 128.582 114.377 L 126.398 114.223 C 124.973 114.122 123.576 114.07 122.205 114.066 Z"
                        ]
                        []
                    , path
                        [ id "frontfill" --fill:#000000;fill-opacity:0.3
                        , style "fill:#000000; stroke: none"
                        , class "fill-front"
                        , d "M 326.806 111.104 C 325.436 111.108 324.04 111.161 322.615 111.262 L 320.43 111.416 L 318.246 111.262 C 316.821 111.161 315.424 111.109 314.054 111.105 C 293.493 111.049 278.714 122.007 259.536 147.781 C 239.08 175.273 218.983 217.581 196.929 262.581 C 174.875 307.582 150.63 355.46 117.138 395.468 C 87.362 431.037 48.7259 460.606 0 471.801 L 0 475.113 L 453.168 475.113 L 453.168 281.265 C 450.037 275.002 446.961 268.764 443.931 262.581 C 421.877 217.581 401.781 175.273 381.325 147.781 C 362.147 122.007 347.367 111.048 326.806 111.104 Z"
                        ]
                        []
                    ]

                else
                    []
               )
            ++ [ path
                    [ id "backstroke1" -- fill:#000000;fill-opacity:1;
                    , style "fill:#000000; stroke: none"
                    , class "stroke-back"
                    , d "M 121.849 49.3349 C 77.8247 49.8899 42.3117 77.7749 17.1836 111.546 C 11.1585 119.644 5.46146 128.186 0 137.061 L 0 275.863 C 1.70648 272.411 3.40309 268.966 5.0811 265.542 C 27.1353 220.542 47.2317 178.234 67.6881 150.742 C 88.1444 123.249 103.597 112.615 126.398 114.223 L 128.582 114.377 L 130.767 114.223 C 151.964 112.728 166.812 121.822 185.231 145.197 C 192.682 132.312 200.531 120.007 209.031 108.584 C 213.408 102.701 218.107 97.0041 223.104 91.5922 C 198.342 65.9293 166.456 47.6317 128.582 49.5312 C 126.323 49.4179 124.065 49.307 121.849 49.3349 Z M 450.576 475.113 C 451.435 475.303 452.303 475.476 453.168 475.654 L 453.168 475.113 Z"
                    ]
                    []
               , path
                    [ id "frontstroke" --fill:#000000;fill-opacity:1
                    , style "fill:#000000; stroke: none"
                    , class "stroke-front"
                    , d "M 313.697 46.3738 C 269.673 46.9288 234.159 74.8127 209.031 108.584 C 182.227 144.607 161.874 189.369 140.205 233.584 C 118.536 277.799 95.7843 321.282 68.9228 353.37 C 48.4326 377.846 26.6872 395.519 0 404.723 L 0 471.801 C 48.7259 460.606 87.362 431.037 117.138 395.468 C 150.63 355.46 174.875 307.581 196.929 262.58 C 218.983 217.58 239.079 175.273 259.535 147.781 C 279.991 120.288 295.444 109.654 318.245 111.262 L 320.43 111.416 L 322.614 111.262 C 345.415 109.654 360.867 120.288 381.324 147.781 C 401.78 175.273 421.875 217.58 443.93 262.58 C 446.959 268.762 450.037 275.001 453.168 281.264 L 453.168 140.987 C 446.452 129.628 439.398 118.757 431.828 108.584 C 405.435 73.1128 367.566 44.205 320.43 46.5689 C 318.171 46.4556 315.913 46.3459 313.697 46.3738 Z"
                    ]
                    []
               ]



-- v3
-- :: [ path
--         [ style "fill:#10b981;fill-opacity:1;stroke:none;"
--         , d "M 112.395 194.007 C 93.1041 193.929 73.938 201.373 60.5984 212.993 C 46.8284 224.988 38.4607 239.743 32.1083 254.813 C 19.4033 284.954 13.9299 318.704 7.50424 351.005 C 1.07853 383.305 -6.2893 413.882 -15.8904 432.52 C -25.4915 451.157 -30.6387 460.752 -46.9227 461.41 L -44.0751 531.908 C -1.55047 530.191 30.5899 496.36 46.8323 464.831 C 63.0747 433.301 70.1055 397.938 76.7038 364.77 C 83.302 331.602 89.4791 300.356 97.1241 282.219 C 100.947 273.15 104.928 267.948 106.942 266.194 C 108.956 264.44 108.197 264.446 112.192 264.563 C 116.025 264.676 115.887 264.742 118.464 267.176 C 121.04 269.61 125.296 275.693 129.287 285.43 C 137.27 304.904 143.616 336.997 150.753 370.821 C 155.293 392.337 179.493 470.035 201.095 531.908 L 273.561 531.908 C 250.478 468.102 224.67 380.543 214.952 356.255 C 202.59 325.357 207.154 289.369 194.571 258.671 C 188.28 243.322 180.141 228.382 166.914 215.888 C 153.688 203.393 134.34 194.627 114.262 194.038 C 113.64 194.019 113.017 194.009 112.395 194.007 Z"
--         ]
--         []
--    ]
-- ++ (if filled then
--         [ path
--             [ style "fill:#10b981;fill-opacity:0.5;stroke:none;"
--             , d "M 109.97 264.521 C 108.403 264.552 108.453 264.878 106.942 266.194 C 104.929 267.948 100.947 273.15 97.1241 282.219 C 89.4791 300.356 83.302 331.602 76.7038 364.77 C 70.1055 397.938 63.0747 433.301 46.8323 464.831 C 30.5899 496.36 -1.55047 530.191 -44.0751 531.908 L 203.351 531.908 C 186.255 490.288 157.166 401.214 150.753 370.821 C 143.616 336.997 137.27 304.904 129.287 285.43 C 125.296 275.693 121.04 269.61 118.464 267.176 C 115.887 264.742 116.025 264.676 112.192 264.563 C 111.193 264.534 110.492 264.511 109.97 264.521 Z"
--             ]
--             []
--         ]
--     else
--         []
--    )
-- ++ [ path
--         [ style "fill:#a855f7;fill-opacity:1;stroke:none;"
--         , d "M 317.131 72.6934 L 316.817 72.6953 C 291.292 72.9231 266.713 86.5449 252.887 104.738 C 239.061 122.932 232.712 143.456 227.795 165.088 C 217.961 208.351 214.919 258.657 205.445 305.834 C 195.972 353.011 180.54 394.956 152.434 422.59 C 124.328 450.224 82.0686 468.544 2.66618 462.785 L -2.43734 533.156 C 90.5855 539.903 158.196 515.871 201.899 472.902 C 245.601 429.934 263.915 373.032 274.619 319.725 C 285.324 266.417 288.893 214.603 296.594 180.727 C 300.444 163.788 305.595 151.988 309.061 147.428 C 312.449 142.969 311.199 143.289 317.133 143.252 C 323.064 143.289 321.815 142.969 325.203 147.428 C 328.669 151.988 333.818 163.788 337.668 180.727 C 345.369 214.603 348.938 266.417 359.643 319.725 C 370.347 373.032 388.661 429.934 432.363 472.902 C 476.066 515.871 543.678 539.903 636.701 533.156 L 631.598 462.785 C 552.195 468.544 509.936 450.224 481.83 422.59 C 453.724 394.956 438.292 353.011 428.819 305.834 C 419.345 258.657 416.303 208.351 406.469 165.088 C 401.552 143.456 395.203 122.932 381.377 104.738 C 367.551 86.5449 342.972 72.9231 317.447 72.6953 L 317.131 72.6934 Z"
--         ]
--         []
--    ]
-- ++ (if filled then
--         [ path
--             [ style "fill:#a855f7;fill-opacity:0.7;stroke:none;"
--             , d "M 317.133 143.252 C 315.649 143.261 314.614 143.248 313.844 143.293 C 313.266 143.326 312.838 143.393 312.479 143.525 C 312.24 143.614 312.033 143.732 311.834 143.889 C 311.735 143.968 311.637 144.057 311.539 144.157 C 310.95 144.756 310.331 145.756 309.061 147.428 C 307.761 149.138 306.224 151.866 304.608 155.501 L 304.608 155.502 C 303.531 157.926 302.417 160.752 301.316 163.949 C 300.215 167.146 299.125 170.714 298.093 174.62 C 297.835 175.596 297.58 176.592 297.33 177.61 C 297.08 178.629 296.834 179.667 296.593 180.726 C 296.353 181.784 296.116 182.861 295.883 183.954 C 295.65 185.047 295.421 186.157 295.195 187.284 C 294.97 188.411 294.747 189.554 294.527 190.713 L 294.527 190.714 C 293.869 194.192 293.237 197.813 292.623 201.565 C 292.214 204.067 291.812 206.627 291.416 209.24 C 291.02 211.854 290.63 214.521 290.241 217.238 C 289.852 219.956 289.466 222.723 289.079 225.537 C 288.306 231.165 287.531 236.975 286.733 242.938 L 286.733 242.939 C 286.334 245.92 285.928 248.939 285.515 251.992 C 285.101 255.046 284.68 258.133 284.246 261.251 C 282.078 276.837 279.626 293.171 276.546 309.751 C 275.93 313.067 275.289 316.393 274.62 319.725 C 274.301 321.313 273.973 322.904 273.64 324.498 C 273.567 324.848 273.492 325.199 273.418 325.549 C 273.188 326.642 272.954 327.735 272.717 328.831 C 272.55 329.6 272.38 330.37 272.21 331.14 C 272.056 331.836 271.9 332.533 271.743 333.229 C 271.549 334.093 271.353 334.956 271.154 335.821 C 271.022 336.39 270.889 336.959 270.756 337.528 C 269.556 342.658 268.267 347.802 266.878 352.946 C 266.86 353.013 266.842 353.08 266.824 353.147 C 255.233 396 236.552 438.831 201.899 472.902 C 168.049 506.183 119.852 528.099 56.6164 533.156 L 577.648 533.156 C 514.412 528.099 466.213 506.183 432.363 472.902 C 397.71 438.831 379.028 396 367.437 353.147 C 367.43 353.122 367.423 353.097 367.417 353.072 C 365.981 347.761 364.654 342.452 363.419 337.157 C 363.329 336.772 363.238 336.386 363.149 336.001 C 362.922 335.016 362.699 334.034 362.478 333.051 C 362.341 332.441 362.205 331.831 362.07 331.222 C 361.891 330.412 361.713 329.603 361.537 328.794 C 361.334 327.858 361.135 326.924 360.938 325.99 C 360.822 325.441 360.704 324.892 360.589 324.344 C 360.267 322.801 359.951 321.261 359.642 319.725 C 358.304 313.061 357.078 306.421 355.94 299.837 C 354.803 293.252 353.755 286.722 352.775 280.281 C 351.795 273.839 350.883 267.485 350.016 261.251 C 349.046 254.275 348.137 247.502 347.25 240.871 C 346.203 233.05 345.188 225.464 344.163 218.258 C 343.727 215.193 343.289 212.175 342.845 209.24 C 342.25 205.32 341.645 201.52 341.018 197.856 C 340.566 195.215 340.101 192.651 339.624 190.155 C 339.257 188.233 338.881 186.355 338.496 184.525 C 338.224 183.238 337.949 181.966 337.668 180.726 C 337.378 179.452 337.08 178.218 336.777 177.003 C 336.324 175.189 335.86 173.432 335.384 171.754 C 333.268 164.286 330.968 158.2 328.855 153.76 C 328.062 152.095 327.296 150.661 326.576 149.473 C 326.096 148.681 325.637 147.998 325.204 147.428 C 324.535 146.549 324.074 145.898 323.676 145.351 C 323.514 145.128 323.343 144.896 323.205 144.719 C 323.037 144.505 322.877 144.313 322.724 144.157 C 322.678 144.11 322.631 144.073 322.584 144.03 C 322.474 143.93 322.361 143.841 322.246 143.766 C 322.204 143.739 322.165 143.706 322.122 143.682 L 322.121 143.682 C 322.015 143.622 321.903 143.57 321.784 143.525 C 321.426 143.393 320.998 143.326 320.42 143.293 C 319.65 143.248 318.615 143.261 317.133 143.252 Z"
--             ]
--             []
--         ]
--     else
--         []
--    )
-- v2
-- -- back line fill
-- :: (if filled then
--         [ path
--             [ style "fill:#10b981;fill-opacity:0.5;fill-rule:evenodd;stroke:none;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;stroke-width:9.999972126"
--             , d "M 19.4204 218.284 C 17.7089 218.274 15.9724 218.278 14.2224 218.288 C 13.7313 218.291 13.2363 218.296 12.7419 218.301 C 11.4472 218.313 10.1409 218.331 8.82495 218.355 C 8.2725 218.365 7.72202 218.374 7.16579 218.386 C 5.37387 218.425 3.56901 218.471 1.73743 218.531 L 0.5446 218.571 L 0.413411 349.939 L 14.9335 356.735 C 44.8699 370.746 66.3509 373.671 82.174 371.612 C 97.9971 369.552 105.796 365.889 122.808 350.256 C 139.816 334.629 157.251 302.192 170.791 271.228 C 160.429 261.485 149.292 252.79 136.529 245.382 C 135.626 244.858 134.713 244.34 133.791 243.827 C 133.602 243.722 133.41 243.619 133.221 243.515 C 132.501 243.117 131.779 242.722 131.047 242.331 C 130.751 242.173 130.451 242.017 130.153 241.86 C 129.188 241.352 128.212 240.849 127.226 240.353 C 126.24 239.857 125.243 239.367 124.235 238.883 C 123.24 238.406 122.234 237.935 121.217 237.47 C 120.598 237.188 119.974 236.908 119.347 236.63 C 119.323 236.619 119.298 236.609 119.274 236.598 C 118.928 236.445 118.582 236.292 118.233 236.141 C 117.575 235.855 116.909 235.573 116.241 235.293 C 115.839 235.124 115.439 234.954 115.033 234.787 C 114.378 234.517 113.715 234.253 113.051 233.989 C 113.047 233.987 113.043 233.985 113.039 233.984 C 112.644 233.827 112.252 233.668 111.854 233.512 C 111.049 233.199 110.234 232.892 109.415 232.586 C 109.383 232.575 109.352 232.563 109.32 232.551 C 109.087 232.464 108.859 232.374 108.625 232.288 C 106.425 231.478 104.171 230.698 101.863 229.947 C 101.857 229.945 101.851 229.944 101.845 229.942 C 101.472 229.82 101.091 229.703 100.716 229.583 C 99.9351 229.335 99.1532 229.086 98.3599 228.845 L 98.3588 228.845 C 97.9388 228.717 97.5123 228.593 97.0888 228.467 C 96.3231 228.239 95.5546 228.012 94.777 227.791 C 94.7602 227.786 94.7431 227.781 94.7263 227.776 C 94.3165 227.66 93.9025 227.546 93.4893 227.431 C 92.6996 227.212 91.9048 226.995 91.1026 226.783 C 91.0926 226.78 91.0827 226.778 91.0728 226.775 C 90.6107 226.653 90.1457 226.533 89.6793 226.413 C 88.8711 226.205 88.0562 226.001 87.2353 225.8 C 86.7881 225.69 86.3402 225.582 85.8892 225.475 C 85.0755 225.281 84.2527 225.091 83.4264 224.905 C 83.403 224.9 83.3793 224.894 83.3558 224.889 C 82.8649 224.778 82.3751 224.667 81.8797 224.558 C 81.0746 224.383 80.2594 224.213 79.4422 224.044 C 79.404 224.036 79.3657 224.027 79.3275 224.019 C 78.8761 223.926 78.4289 223.832 77.9738 223.74 C 76.9099 223.527 75.8315 223.322 74.7469 223.12 C 74.4559 223.066 74.1696 223.009 73.8771 222.955 C 71.0803 222.446 68.2157 221.973 65.2771 221.541 C 65.0748 221.511 64.8671 221.484 64.6641 221.455 C 63.4188 221.275 62.1632 221.101 60.8916 220.935 C 60.4886 220.882 60.078 220.834 59.6723 220.783 C 58.5761 220.645 57.4743 220.51 56.3584 220.382 C 55.8583 220.325 55.3511 220.272 54.847 220.217 C 53.8326 220.107 52.813 220 51.7822 219.898 C 51.2062 219.842 50.6249 219.788 50.0437 219.734 C 49.0104 219.639 47.9703 219.546 46.9205 219.46 C 46.3653 219.414 45.807 219.371 45.247 219.327 C 44.1786 219.245 43.1015 219.167 42.0158 219.094 C 41.4378 219.055 40.8591 219.016 40.2762 218.98 C 39.2403 218.916 38.1925 218.858 37.1409 218.803 C 36.5 218.769 35.8616 218.733 35.2149 218.702 L 35.2094 218.702 C 34.1079 218.65 32.9923 218.606 31.8734 218.563 C 31.2758 218.541 30.6836 218.515 30.0809 218.495 C 28.6382 218.447 27.1781 218.409 25.7065 218.377 C 25.4491 218.371 25.1974 218.362 24.9392 218.357 C 23.1215 218.321 21.2828 218.296 19.4204 218.284 Z M 221.47 329.336 C 208.198 355.349 192.528 379.701 172.098 399.959 C 151.62 420.266 124.919 436.008 93.6194 440.683 L 448.322 440.683 L 448.381 439.529 C 409.038 440.334 375.558 435.222 347.037 425.763 C 345.539 425.266 344.055 424.758 342.584 424.237 C 342.446 424.188 342.31 424.138 342.172 424.088 C 340.846 423.616 339.527 423.136 338.222 422.645 C 338.024 422.571 337.83 422.493 337.632 422.418 C 336.388 421.946 335.149 421.468 333.925 420.978 C 333.848 420.948 333.772 420.916 333.696 420.885 C 333.545 420.823 333.394 420.761 333.242 420.699 C 332.048 420.217 330.859 419.73 329.683 419.232 C 329.47 419.142 329.261 419.048 329.049 418.958 C 327.884 418.46 326.724 417.959 325.577 417.446 C 325.238 417.294 324.904 417.137 324.566 416.983 C 323.253 416.387 321.952 415.782 320.663 415.166 C 319.595 414.656 318.531 414.145 317.48 413.622 C 317.18 413.473 316.887 413.318 316.588 413.168 C 315.582 412.661 314.577 412.152 313.585 411.633 C 312.291 410.956 311.008 410.268 309.74 409.57 C 309.569 409.476 309.401 409.38 309.232 409.285 C 309 409.154 308.768 409.023 308.536 408.893 C 303.887 406.297 299.401 403.574 295.087 400.719 C 263.515 379.826 241.288 353.866 221.551 329.457 C 221.524 329.415 221.497 329.377 221.47 329.336 Z M 0.336241 426.689 L 0.321909 440.683 L 51.2398 440.683 C 34.9314 438.455 17.9655 433.77 0.336241 426.689 Z"
--             ]
--             []
--         ]
--     else
--         []
--    )
-- ++ -- back line stroke
--    [ path
--         [ style "fill:#10b981;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:0.999998;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:1;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;color-rendering:auto;image-rendering:auto;shape-rendering:auto;text-rendering:auto;enable-background:accumulate;stop-color:#000000;stop-opacity:1"
--         , d "m 221.55117,329.45701 c 19.73729,24.40961 41.96474,50.36869 73.5369,71.26134 37.86901,25.05953 88.3668,40.60741 156.0625,38.75586 l 34.98633,-0.95704 -1.91406,-69.97265 -34.98633,0.95703 c -56.6948,1.55067 -89.55965,-9.97944 -115.51953,-27.1582 -25.95988,-17.17877 -45.83225,-42.22955 -69.29101,-71.25391 -3.61725,-4.47544 -7.32636,-9.04207 -11.1669,-13.65468"
--         ]
--         []
--    -- back line stroke
--    , path
--         [ style "fill:#10b981;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:0.999998;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:1;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;color-rendering:auto;image-rendering:auto;shape-rendering:auto;text-rendering:auto;enable-background:accumulate;stop-color:#000000;stop-opacity:1"
--         , d "m 199.20461,203.3723 c -8.514,-6.63348 -17.71818,-12.95027 -27.76251,-18.7799 -36.73137,-21.31851 -83.676809,-35.00063 -145.046773,-36.42541 -8.76713,-0.20354 -17.8299798,-0.1559 -27.19805892,0.15434 l -34.98012108,1.15755 2.31621,69.96134 34.9801208,-1.15755 c 65.2048592,-2.15934 105.0622922,9.59601 134.7919522,26.8508 12.85988,7.46374 24.07013,16.23316 34.49836,26.06587 z"
--         ]
--         []
--    -- front line fill
--    ]
-- ++ (if filled then
--         [ path
--             [ style "fill:#a855f7;fill-opacity:0.5;fill-rule:evenodd;stroke:none;stroke-width:9.99998;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
--             , d "m 375.29297,91.881901 c -0.82335,0.0031 -1.62854,0.02354 -2.42424,0.05292 -0.40336,0.01489 -0.80419,0.03264 -1.20055,0.05402 -0.22345,0.01206 -0.44573,0.02451 -0.66697,0.03858 -0.002,1.19e-4 -0.004,-1.13e-4 -0.006,0 -0.59183,0.03771 -1.17775,0.08164 -1.75397,0.133395 -0.003,2.37e-4 -0.005,8.7e-4 -0.008,0.0011 -0.55157,0.0496 -1.09741,0.103411 -1.6349,0.165359 -15.48581,1.784841 -27.08403,8.032115 -39.65774,20.418115 -25.14741,24.77199 -47.76008,76.19203 -69.99,131.37113 -22.22992,55.1791 -44.14561,114.11184 -86.15054,155.76572 -1.08119,1.07216 -2.18394,2.12812 -3.29957,3.1739 -0.19643,0.18532 -0.39165,0.37175 -0.58981,0.55562 -0.42306,0.39262 -0.852,0.77995 -1.27991,1.16858 -0.12406,0.11266 -0.24797,0.2254 -0.37153,0.33845 -0.11913,0.109 -0.23789,0.21832 -0.35828,0.32631 -0.37735,0.33848 -0.75999,0.67233 -1.14102,1.00763 -0.31213,0.27465 -0.62247,0.55104 -0.93706,0.82351 -0.39027,0.33803 -0.78557,0.6719 -1.17961,1.00652 -0.29121,0.24727 -0.58096,0.49547 -0.87423,0.74084 -0.42664,0.35701 -0.85776,0.70982 -1.28874,1.06274 -0.27579,0.2258 -0.55036,0.45279 -0.82792,0.67689 -0.39703,0.32061 -0.79772,0.63765 -1.19834,0.9547 -0.70638,0.55903 -1.41794,1.11259 -2.13541,1.66026 -0.17089,0.13045 -0.34259,0.26007 -0.51373,0.39027 -0.17072,0.12988 -0.34088,0.26034 -0.51263,0.38915 -0.43769,0.32833 -0.87899,0.65277 -1.32072,0.97676 -0.28982,0.21253 -0.57936,0.42546 -0.87092,0.6361 -0.46145,0.33345 -0.9266,0.66246 -1.39236,0.99108 -0.27806,0.19614 -0.55607,0.39211 -0.83565,0.58649 -0.47541,0.33063 -0.95334,0.65789 -1.43315,0.98337 -0.16335,0.1108 -0.32694,0.22141 -0.49059,0.33184 -0.12115,0.0818 -0.24222,0.1635 -0.3638,0.24473 -0.49617,0.3316 -0.99521,0.65958 -1.496,0.98558 -0.25521,0.16609 -0.51089,0.33148 -0.76729,0.49609 -0.60115,0.38606 -1.20582,0.76762 -1.8135,1.14543 -0.17225,0.10667 -0.34444,0.21341 -0.51704,0.3197 -0.53871,0.33182 -1.07902,0.66147 -1.62278,0.98668 -0.24372,0.14572 -0.48837,0.28997 -0.73311,0.43436 -0.61355,0.36205 -1.23002,0.7203 -1.84988,1.07376 -0.95691,0.54566 -1.92204,1.07941 -2.89388,1.60404 -0.63166,0.34099 -1.26489,0.67904 -1.9028,1.01093 -0.0723,0.0376 -0.14464,0.0752 -0.21717,0.11245 -0.0746,0.0383 -0.1493,0.0762 -0.2238,0.11465 -0.75217,0.38795 -1.50807,0.77041 -2.2688,1.14542 -0.0183,0.009 -0.0368,0.0175 -0.0551,0.0265 -11.50569,5.66564 -23.98562,9.83257 -37.313965,11.99334 H 447.80018 l 1.44088,-324.72201 -15.15842,-7.38518 c -3.1e-4,-1.4e-4 -8e-4,1.5e-4 -0.001,0 -1.43258,-0.69794 -2.83964,-1.36226 -4.23333,-2.00863 -0.20754,-0.0962 -0.41618,-0.1948 -0.62287,-0.28994 -0.017,-0.008 -0.0337,-0.0142 -0.0507,-0.0221 -1.317,-0.60577 -2.61562,-1.18882 -3.89819,-1.74955 -0.26089,-0.11407 -0.51993,-0.22512 -0.77942,-0.33735 -1.29186,-0.55873 -2.57047,-1.10215 -3.82763,-1.61616 -0.0667,-0.0273 -0.13186,-0.0522 -0.19845,-0.0794 -0.002,-7.7e-4 -0.004,-0.001 -0.005,-0.002 -1.2717,-0.5183 -2.52264,-1.01012 -3.7593,-1.48387 -0.17481,-0.067 -0.35173,-0.13566 -0.52585,-0.20175 -1.20899,-0.45892 -2.39936,-0.895579 -3.57518,-1.312999 -0.21389,-0.0759 -0.42662,-0.15031 -0.63942,-0.22489 -0.003,-0.001 -0.007,-0.002 -0.01,-0.003 -1.19912,-0.420169 -2.3849,-0.823559 -3.54983,-1.201648 -0.0986,-0.03199 -0.19492,-0.06201 -0.29324,-0.09371 -0.0179,-0.0058 -0.035,-0.01077 -0.0529,-0.01653 -1.08205,-0.34837 -2.14857,-0.676927 -3.20145,-0.989985 -0.1778,-0.05287 -0.35771,-0.109079 -0.53468,-0.160957 -1.09664,-0.321457 -2.17476,-0.620906 -3.24005,-0.905092 -0.19595,-0.05227 -0.39159,-0.103305 -0.58649,-0.154342 -1.07029,-0.280269 -2.12725,-0.545626 -3.16618,-0.789341 -0.14995,-0.03517 -0.29719,-0.06699 -0.44649,-0.10142 -0.98383,-0.226878 -1.95441,-0.438822 -2.91041,-0.633899 -0.10971,-0.02239 -0.22139,-0.04748 -0.33073,-0.06945 -3.6e-4,-7.3e-5 -7.4e-4,7.3e-5 -10e-4,0 -1.01305,-0.203595 -2.0076,-0.38547 -2.98978,-0.554522 -0.17153,-0.02952 -0.34314,-0.0597 -0.51374,-0.08819 -7.3e-4,-1.24e-4 -10e-4,1.24e-4 -0.002,0 -0.96729,-0.161533 -1.92096,-0.308514 -2.85861,-0.437665 -0.13645,-0.0188 -0.27096,-0.03479 -0.40679,-0.05292 -1.92695,-0.257127 -3.79153,-0.447356 -5.59594,-0.575468 -0.16077,-0.01141 -0.32195,-0.02374 -0.48176,-0.03418 -0.85586,-0.0559 -1.69903,-0.09816 -2.52787,-0.12678 -1.00993,-0.03487 -2.00261,-0.05327 -2.97326,-0.04961 z M -0.19927194,426.51697 v 13.83991 H 49.183008 c -5.88127,-0.86452 -11.84747,-2.04746 -17.89907,-3.54431 -0.0254,-0.006 -0.0507,-0.0125 -0.0761,-0.0187 -0.96448,-0.23896 -1.93168,-0.48701 -2.9005,-0.74194 -0.14187,-0.0373 -0.28357,-0.0737 -0.42554,-0.11134 -1.92388,-0.51053 -3.85672,-1.05282 -5.79768,-1.62609 -0.16785,-0.0496 -0.33583,-0.0999 -0.50381,-0.14993 -0.88616,-0.264 -1.77376,-0.53326 -2.66347,-0.81029 -0.0598,-0.0186 -0.11986,-0.0372 -0.1797,-0.0562 -1.00768,-0.31466 -2.01723,-0.6366 -3.02948,-0.96793 -0.10311,-0.0337 -0.20662,-0.0686 -0.30978,-0.10253 -5.14035,-1.69036 -10.3390305,-3.59289 -15.59718994,-5.71059 z m 93.39901894,14.1067 c -0.76129,0.11296 -1.523039,0.22545 -2.289748,0.32522 0.766609,-0.0998 1.529308,-0.21021 2.289748,-0.32522 z m -2.289748,0.32522 c -0.93118,0.12117 -1.865199,0.23062 -2.801278,0.33294 0.936169,-0.10227 1.870028,-0.21176 2.801278,-0.33294 z m -39.264161,-0.25025 c 0.13452,0.0178 0.26905,0.0365 0.40349,0.054 -0.13445,-0.0175 -0.26895,-0.0362 -0.40349,-0.054 z m 2.72631,0.33955 c 0.17364,0.0202 0.34685,0.0399 0.52034,0.0595 -0.17345,-0.0196 -0.34674,-0.0394 -0.52034,-0.0595 z m 32.778574,0.34175 c -0.62302,0.0637 -1.246759,0.12429 -1.871938,0.17969 0.625069,-0.0553 1.249028,-0.11604 1.871938,-0.17969 z m -2.956737,0.2712 c -0.59788,0.0483 -1.197109,0.0915 -1.796959,0.13229 0.59995,-0.0407 1.198999,-0.0841 1.796959,-0.13229 z m -2.925848,0.20615 c -0.582489,0.0349 -1.166309,0.0637 -1.750649,0.0915 0.58424,-0.0277 1.16827,-0.0567 1.750649,-0.0915 z m -16.691889,0.043 c 0.27896,0.0147 0.55817,0.0309 0.83674,0.0441 -0.2785,-0.0131 -0.55785,-0.0295 -0.83674,-0.0441 z m 13.79802,0.10252 c -0.76117,0.0302 -1.52435,0.0523 -2.28865,0.0706 0.76445,-0.0181 1.52732,-0.0405 2.28865,-0.0706 z m -11.19629,0.0132 c 0.39317,0.0141 0.78612,0.0264 1.1785,0.0375 -0.39242,-0.011 -0.78528,-0.0235 -1.1785,-0.0375 z m 2.80017,0.0761 c 0.43872,0.008 0.87639,0.0105 1.3141,0.0143 -0.43762,-0.004 -0.87547,-0.007 -1.3141,-0.0143 z m 4.38547,0.009 c -0.5027,0.006 -1.00518,0.0134 -1.50923,0.0143 0.50401,-8.5e-4 1.00657,-0.008 1.50923,-0.0143 z"
--             ]
--             []
--         ]
--     else
--         []
--    )
-- ++ -- front line stroke
--    [ path
--         [ style "fill:#a855f7;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-width:0.999998;stroke-linecap:square;stroke-linejoin:miter;stroke-miterlimit:1;stroke-dasharray:none;stroke-dashoffset:0;stroke-opacity:1;color-rendering:auto;image-rendering:auto;shape-rendering:auto;text-rendering:auto;enable-background:accumulate;stop-color:#000000;stop-opacity:1"
--         , d "m 374.80746,21.966252 c -0.90451,0.006 -1.80721,0.0206 -2.70757,0.0441 -4.11594,0.10725 -8.18827,0.39101 -12.21824,0.85548 -32.23976,3.71585 -59.82923,19.46322 -80.76626,40.08769 C 240.92965,100.5692 219.53286,152.652 199.20461,203.3723 l -28.40082,67.82704 c -13.5413,30.97336 -30.98256,63.4253 -47.99541,79.0575 -17.01285,15.6322 -24.811358,19.29617 -40.634474,21.35518 -15.823129,2.05902 -37.304159,-0.86529 -67.240549,-14.8762 l -31.698179,-14.83651 -29.671921,63.39857 31.699291,14.83541 c 38.306489,17.9283 73.601849,25.10194 105.94466,20.89327 32.342822,-4.20868 59.888372,-20.2397 80.890832,-41.06664 42.00493,-41.65388 63.92062,-100.58663 86.15054,-155.76572 22.22992,-55.1791 44.8426,-106.59914 69.99001,-131.37113 12.57371,-12.386 24.17192,-18.633278 39.65773,-20.418118 15.4858,-1.78484 36.77029,1.44573 66.48428,15.922408 l 31.46557,15.32929 30.65749,-62.928938 -31.46447,-15.32819 c -32.16153,-15.66911 -62.19214,-23.63333 -90.23173,-23.43326 z"
--         ]
--         []
--    ]
-- v1
-- :: (if filled then
--         [ path
--             --c6cad1
--             [ style "fill:#000000;fill-rule:evenodd;stroke:none;stroke-width:0;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
--             , d "M 0,195.20001 C 153.64908,235.99698 177.55101,31.147539 238.14123,31.147539 c 60.59022,0 61.3719,227.131241 209.85876,164.052471 L 448 472.209 L 0 472.209  Z"
--             ]
--             []
--         ]
--     else
--         []
--    )
-- ++ [ path
--         [ style "fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:75px;stroke-linecap:square;stroke-linejoin:miter;"
--         , d "M -3.5e-6,195.20001 C 153.64908,235.99698 130.62149,31.14754 223.46567,31.14754 c 92.84419,0 56.69508,204.55346 224.53432,164.05247"
--         ]
--         []
--    ]
