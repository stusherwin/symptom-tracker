module Svg.Icon exposing (IconType(..), all, decode, encode, fromString, icon, iconSymbols, logo, toString, userSelectable)

import Html exposing (p)
import Json.Decode as D
import Json.Encode as E
import Svg exposing (Svg, g, path, svg, symbol, use)
import Svg.Attributes exposing (class, d, id, style, transform, viewBox, xlinkHref)


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
    | SolidPlusCircle
    | SolidTrash
    | SolidTrashBan
    | SolidQuestionCircle
    | SolidEye
    | SolidEyeSlash
    | SolidArrowUp
    | SolidArrowDown
    | SolidCrosshairs


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

        _ ->
            Nothing


iconSymbol : IconType -> Svg msg
iconSymbol iconType =
    case iconType of
        SolidCheckCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M504 256c0 136.967-111.033 248-248 248S8 392.967 8 256 119.033 8 256 8s248 111.033 248 248zM227.314 387.314l184-184c6.248-6.248 6.248-16.379 0-22.627l-22.627-22.627c-6.248-6.249-16.379-6.249-22.628 0L216 308.118l-70.059-70.059c-6.248-6.248-16.379-6.248-22.628 0l-22.627 22.627c-6.248 6.248-6.248 16.379 0 22.627l104 104c6.249 6.249 16.379 6.249 22.628.001z" ] []
                ]

        SolidTimesCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm121.6 313.1c4.7 4.7 4.7 12.3 0 17L338 377.6c-4.7 4.7-12.3 4.7-17 0L256 312l-65.1 65.6c-4.7 4.7-12.3 4.7-17 0L134.4 338c-4.7-4.7-4.7-12.3 0-17l65.6-65-65.6-65.1c-4.7-4.7-4.7-12.3 0-17l39.6-39.6c4.7-4.7 12.3-4.7 17 0l65 65.7 65.1-65.6c4.7-4.7 12.3-4.7 17 0l39.6 39.6c4.7 4.7 4.7 12.3 0 17L312 256l65.6 65.1z" ] []
                ]

        SolidGrin ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path [ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm80 168c17.7 0 32 14.3 32 32s-14.3 32-32 32-32-14.3-32-32 14.3-32 32-32zm-160 0c17.7 0 32 14.3 32 32s-14.3 32-32 32-32-14.3-32-32 14.3-32 32-32zm80 256c-60.6 0-134.5-38.3-143.8-93.3-2-11.8 9.3-21.6 20.7-17.9C155.1 330.5 200 336 248 336s92.9-5.5 123.1-15.2c11.3-3.7 22.6 6.1 20.7 17.9-9.3 55-83.2 93.3-143.8 93.3z" ] []
                ]

        SolidFrownOpen ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path [ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zM136 208c0-17.7 14.3-32 32-32s32 14.3 32 32-14.3 32-32 32-32-14.3-32-32zm187.3 183.3c-31.2-9.6-59.4-15.3-75.3-15.3s-44.1 5.7-75.3 15.3c-11.5 3.5-22.5-6.3-20.5-18.1 7-40 60.1-61.2 95.8-61.2s88.8 21.3 95.8 61.2c2 11.9-9.1 21.6-20.5 18.1zM328 240c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32z" ] []
                ]

        SolidMeh ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path [ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm-80 168c17.7 0 32 14.3 32 32s-14.3 32-32 32-32-14.3-32-32 14.3-32 32-32zm176 192H152c-21.2 0-21.2-32 0-32h192c21.2 0 21.2 32 0 32zm-16-128c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32z" ] []
                ]

        SolidLaughBeam ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path [ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm24 199.4c3.3-42.1 32.2-71.4 56-71.4s52.7 29.3 56 71.4c.7 8.6-10.8 11.9-14.9 4.5l-9.5-17c-7.7-13.7-19.2-21.6-31.5-21.6s-23.8 7.9-31.5 21.6l-9.5 17c-4.2 7.4-15.8 4.1-15.1-4.5zm-160 0c3.3-42.1 32.2-71.4 56-71.4s52.7 29.3 56 71.4c.7 8.6-10.8 11.9-14.9 4.5l-9.5-17c-7.7-13.7-19.2-21.6-31.5-21.6s-23.8 7.9-31.5 21.6l-9.5 17c-4.3 7.4-15.8 4-15.1-4.5zM398.9 306C390 377 329.4 432 256 432h-16c-73.4 0-134-55-142.9-126-1.2-9.5 6.3-18 15.9-18h270c9.6 0 17.1 8.4 15.9 18z" ] []
                ]

        SolidTired ->
            symbol [ id <| toString iconType, viewBox "0 0 496 512" ]
                [ path [ d "M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm33.8 189.7l80-48c11.6-6.9 24 7.7 15.4 18L343.6 208l33.6 40.3c8.7 10.4-3.9 24.8-15.4 18l-80-48c-7.7-4.7-7.7-15.9 0-20.6zm-163-30c-8.6-10.3 3.8-24.9 15.4-18l80 48c7.8 4.7 7.8 15.9 0 20.6l-80 48c-11.5 6.8-24-7.6-15.4-18l33.6-40.3-33.6-40.3zM248 288c51.9 0 115.3 43.8 123.2 106.7 1.7 13.6-8 24.6-17.7 20.4-25.9-11.1-64.4-17.4-105.5-17.4s-79.6 6.3-105.5 17.4c-9.8 4.2-19.4-7-17.7-20.4C132.7 331.8 196.1 288 248 288z" ] []
                ]

        SolidAngleDoubleLeft ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M223.7 239l136-136c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9L319.9 256l96.4 96.4c9.4 9.4 9.4 24.6 0 33.9L393.7 409c-9.4 9.4-24.6 9.4-33.9 0l-136-136c-9.5-9.4-9.5-24.6-.1-34zm-192 34l136 136c9.4 9.4 24.6 9.4 33.9 0l22.6-22.6c9.4-9.4 9.4-24.6 0-33.9L127.9 256l96.4-96.4c9.4-9.4 9.4-24.6 0-33.9L201.7 103c-9.4-9.4-24.6-9.4-33.9 0l-136 136c-9.5 9.4-9.5 24.6-.1 34z" ] []
                ]

        SolidAngleDoubleRight ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M224.3 273l-136 136c-9.4 9.4-24.6 9.4-33.9 0l-22.6-22.6c-9.4-9.4-9.4-24.6 0-33.9l96.4-96.4-96.4-96.4c-9.4-9.4-9.4-24.6 0-33.9L54.3 103c9.4-9.4 24.6-9.4 33.9 0l136 136c9.5 9.4 9.5 24.6.1 34zm192-34l-136-136c-9.4-9.4-24.6-9.4-33.9 0l-22.6 22.6c-9.4 9.4-9.4 24.6 0 33.9l96.4 96.4-96.4 96.4c-9.4 9.4-9.4 24.6 0 33.9l22.6 22.6c9.4 9.4 24.6 9.4 33.9 0l136-136c9.4-9.2 9.4-24.4 0-33.8z" ] []
                ]

        SolidAngleLeft ->
            symbol [ id <| toString iconType, viewBox "0 0 256 512" ]
                [ path [ d "M31.7 239l136-136c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9L127.9 256l96.4 96.4c9.4 9.4 9.4 24.6 0 33.9L201.7 409c-9.4 9.4-24.6 9.4-33.9 0l-136-136c-9.5-9.4-9.5-24.6-.1-34z" ] []
                ]

        SolidAngleRight ->
            symbol [ id <| toString iconType, viewBox "0 0 256 512" ]
                [ path [ d "M224.3 273l-136 136c-9.4 9.4-24.6 9.4-33.9 0l-22.6-22.6c-9.4-9.4-9.4-24.6 0-33.9l96.4-96.4-96.4-96.4c-9.4-9.4-9.4-24.6 0-33.9L54.3 103c9.4-9.4 24.6-9.4 33.9 0l136 136c9.5 9.4 9.5 24.6.1 34z" ] []
                ]

        SolidBars ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M16 132h416c8.837 0 16-7.163 16-16V76c0-8.837-7.163-16-16-16H16C7.163 60 0 67.163 0 76v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16z" ] []
                ]

        SolidTimes ->
            symbol [ id <| toString iconType, viewBox "0 0 352 512" ]
                [ path [ d "M242.72 256l100.07-100.07c12.28-12.28 12.28-32.19 0-44.48l-22.24-22.24c-12.28-12.28-32.19-12.28-44.48 0L176 189.28 75.93 89.21c-12.28-12.28-32.19-12.28-44.48 0L9.21 111.45c-12.28 12.28-12.28 32.19 0 44.48L109.28 256 9.21 356.07c-12.28 12.28-12.28 32.19 0 44.48l22.24 22.24c12.28 12.28 32.2 12.28 44.48 0L176 322.72l100.07 100.07c12.28 12.28 32.2 12.28 44.48 0l22.24-22.24c12.28-12.28 12.28-32.19 0-44.48L242.72 256z" ] []
                ]

        SolidChartLine ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M496 384H64V80c0-8.84-7.16-16-16-16H16C7.16 64 0 71.16 0 80v336c0 17.67 14.33 32 32 32h464c8.84 0 16-7.16 16-16v-32c0-8.84-7.16-16-16-16zM464 96H345.94c-21.38 0-32.09 25.85-16.97 40.97l32.4 32.4L288 242.75l-73.37-73.37c-12.5-12.5-32.76-12.5-45.25 0l-68.69 68.69c-6.25 6.25-6.25 16.38 0 22.63l22.62 22.62c6.25 6.25 16.38 6.25 22.63 0L192 237.25l73.37 73.37c12.5 12.5 32.76 12.5 45.25 0l96-96 32.4 32.4c15.12 15.12 40.97 4.41 40.97-16.97V112c.01-8.84-7.15-16-15.99-16z" ] []
                ]

        SolidCog ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M487.4 315.7l-42.6-24.6c4.3-23.2 4.3-47 0-70.2l42.6-24.6c4.9-2.8 7.1-8.6 5.5-14-11.1-35.6-30-67.8-54.7-94.6-3.8-4.1-10-5.1-14.8-2.3L380.8 110c-17.9-15.4-38.5-27.3-60.8-35.1V25.8c0-5.6-3.9-10.5-9.4-11.7-36.7-8.2-74.3-7.8-109.2 0-5.5 1.2-9.4 6.1-9.4 11.7V75c-22.2 7.9-42.8 19.8-60.8 35.1L88.7 85.5c-4.9-2.8-11-1.9-14.8 2.3-24.7 26.7-43.6 58.9-54.7 94.6-1.7 5.4.6 11.2 5.5 14L67.3 221c-4.3 23.2-4.3 47 0 70.2l-42.6 24.6c-4.9 2.8-7.1 8.6-5.5 14 11.1 35.6 30 67.8 54.7 94.6 3.8 4.1 10 5.1 14.8 2.3l42.6-24.6c17.9 15.4 38.5 27.3 60.8 35.1v49.2c0 5.6 3.9 10.5 9.4 11.7 36.7 8.2 74.3 7.8 109.2 0 5.5-1.2 9.4-6.1 9.4-11.7v-49.2c22.2-7.9 42.8-19.8 60.8-35.1l42.6 24.6c4.9 2.8 11 1.9 14.8-2.3 24.7-26.7 43.6-58.9 54.7-94.6 1.5-5.5-.7-11.3-5.6-14.1zM256 336c-44.1 0-80-35.9-80-80s35.9-80 80-80 80 35.9 80 80-35.9 80-80 80z" ] []
                ]

        SolidCalendarDay ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M0 464c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V192H0v272zm64-192c0-8.8 7.2-16 16-16h96c8.8 0 16 7.2 16 16v96c0 8.8-7.2 16-16 16H80c-8.8 0-16-7.2-16-16v-96zM400 64h-48V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H160V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H48C21.5 64 0 85.5 0 112v48h448v-48c0-26.5-21.5-48-48-48z" ] []
                ]

        SolidCalendarAlt ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M0 464c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V192H0v272zm320-196c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zm0 128c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zM192 268c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zm0 128c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12h-40c-6.6 0-12-5.4-12-12v-40zM64 268c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H76c-6.6 0-12-5.4-12-12v-40zm0 128c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H76c-6.6 0-12-5.4-12-12v-40zM400 64h-48V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H160V16c0-8.8-7.2-16-16-16h-32c-8.8 0-16 7.2-16 16v48H48C21.5 64 0 85.5 0 112v48h448v-48c0-26.5-21.5-48-48-48z" ] []
                ]

        SolidAngleDown ->
            symbol [ id <| toString iconType, viewBox "0 0 320 512" ]
                [ path [ d "M143 352.3L7 216.3c-9.4-9.4-9.4-24.6 0-33.9l22.6-22.6c9.4-9.4 24.6-9.4 33.9 0l96.4 96.4 96.4-96.4c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9l-136 136c-9.2 9.4-24.4 9.4-33.8 0z" ] []
                ]

        SolidPlusCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm144 276c0 6.6-5.4 12-12 12h-92v92c0 6.6-5.4 12-12 12h-56c-6.6 0-12-5.4-12-12v-92h-92c-6.6 0-12-5.4-12-12v-56c0-6.6 5.4-12 12-12h92v-92c0-6.6 5.4-12 12-12h56c6.6 0 12 5.4 12 12v92h92c6.6 0 12 5.4 12 12v56z" ] []
                ]

        SolidTrash ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M432 32H312l-9.4-18.7A24 24 0 0 0 281.1 0H166.8a23.72 23.72 0 0 0-21.4 13.3L136 32H16A16 16 0 0 0 0 48v32a16 16 0 0 0 16 16h416a16 16 0 0 0 16-16V48a16 16 0 0 0-16-16zM53.2 467a48 48 0 0 0 47.9 45h245.8a48 48 0 0 0 47.9-45L416 128H32z" ] []
                ]

        SolidTrashBan ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M 198.8 0.00035 C 189.706 -0.0394213 181.391 5.12832 177.4 13.3004 L 168 32.0004 L 48.0001 32.0004 C 43.7566 32.0004 39.687 33.6861 36.6864 36.6867 C 33.6858 39.6873 32.0001 43.757 32.0001 48.0004 L 32.0001 80.0005 C 32.0001 84.244 33.6858 88.3136 36.6864 91.3142 C 39.687 94.3148 43.7566 96.0005 48.0001 96.0005 L 464.001 96.0005 C 468.244 96.0005 472.314 94.3148 475.315 91.3142 C 478.315 88.3136 480.001 84.244 480.001 80.0005 L 480.001 48.0004 C 480.001 43.757 478.315 39.6873 475.315 36.6867 C 472.314 33.6861 468.244 32.0004 464.001 32.0004 L 344.001 32.0004 L 334.601 13.3004 C 330.539 5.14579 322.211 -0.00618451 313.101 0.00035 L 198.8 0.00035 Z M 64.0001 128.001 L 85.2002 467.001 C 86.7841 492.294 107.758 511.998 133.1 512.001 L 378.901 512.001 C 404.243 511.998 425.217 492.294 426.801 467.001 L 448.001 128.001 L 64.0001 128.001 Z M 256 169.472 C 339.338 169.472 406.858 233.345 406.858 312.182 C 406.858 391.019 339.338 454.896 256 454.896 C 172.663 454.896 105.143 391.019 105.143 312.182 C 105.143 233.345 172.663 169.472 256 169.472 Z M 255 206.286 C 232.659 206.488 210.507 212.969 191.824 225.406 L 347.734 372.892 C 377.723 332.552 374.922 274.95 335.139 237.315 C 312.761 216.146 283.725 206.028 255 206.286 Z M 164.267 251.472 C 134.278 291.812 137.079 349.414 176.862 387.049 C 216.645 424.683 277.474 427.386 320.177 398.958 L 164.267 251.472 Z" ] []
                ]

        SolidQuestionCircle ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M504 256c0 136.997-111.043 248-248 248S8 392.997 8 256C8 119.083 119.043 8 256 8s248 111.083 248 248zM262.655 90c-54.497 0-89.255 22.957-116.549 63.758-3.536 5.286-2.353 12.415 2.715 16.258l34.699 26.31c5.205 3.947 12.621 3.008 16.665-2.122 17.864-22.658 30.113-35.797 57.303-35.797 20.429 0 45.698 13.148 45.698 32.958 0 14.976-12.363 22.667-32.534 33.976C247.128 238.528 216 254.941 216 296v4c0 6.627 5.373 12 12 12h56c6.627 0 12-5.373 12-12v-1.333c0-28.462 83.186-29.647 83.186-106.667 0-58.002-60.165-102-116.531-102zM256 338c-25.365 0-46 20.635-46 46 0 25.364 20.635 46 46 46s46-20.636 46-46c0-25.365-20.635-46-46-46z" ] []
                ]

        SolidEye ->
            symbol [ id <| toString iconType, viewBox "0 0 576 512" ]
                [ path [ d "M572.52 241.4C518.29 135.59 410.93 64 288 64S57.68 135.64 3.48 241.41a32.35 32.35 0 0 0 0 29.19C57.71 376.41 165.07 448 288 448s230.32-71.64 284.52-177.41a32.35 32.35 0 0 0 0-29.19zM288 400a144 144 0 1 1 144-144 143.93 143.93 0 0 1-144 144zm0-240a95.31 95.31 0 0 0-25.31 3.79 47.85 47.85 0 0 1-66.9 66.9A95.78 95.78 0 1 0 288 160z" ] []
                ]

        SolidEyeSlash ->
            symbol [ id <| toString iconType, viewBox "0 0 640 512" ]
                [ path [ d "M320 400c-75.85 0-137.25-58.71-142.9-133.11L72.2 185.82c-13.79 17.3-26.48 35.59-36.72 55.59a32.35 32.35 0 0 0 0 29.19C89.71 376.41 197.07 448 320 448c26.91 0 52.87-4 77.89-10.46L346 397.39a144.13 144.13 0 0 1-26 2.61zm313.82 58.1l-110.55-85.44a331.25 331.25 0 0 0 81.25-102.07 32.35 32.35 0 0 0 0-29.19C550.29 135.59 442.93 64 320 64a308.15 308.15 0 0 0-147.32 37.7L45.46 3.37A16 16 0 0 0 23 6.18L3.37 31.45A16 16 0 0 0 6.18 53.9l588.36 454.73a16 16 0 0 0 22.46-2.81l19.64-25.27a16 16 0 0 0-2.82-22.45zm-183.72-142l-39.3-30.38A94.75 94.75 0 0 0 416 256a94.76 94.76 0 0 0-121.31-92.21A47.65 47.65 0 0 1 304 192a46.64 46.64 0 0 1-1.54 10l-73.61-56.89A142.31 142.31 0 0 1 320 112a143.92 143.92 0 0 1 144 144c0 21.63-5.29 41.79-13.9 60.11z" ] []
                ]

        SolidArrowDown ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M413.1 222.5l22.2 22.2c9.4 9.4 9.4 24.6 0 33.9L241 473c-9.4 9.4-24.6 9.4-33.9 0L12.7 278.6c-9.4-9.4-9.4-24.6 0-33.9l22.2-22.2c9.5-9.5 25-9.3 34.3.4L184 343.4V56c0-13.3 10.7-24 24-24h32c13.3 0 24 10.7 24 24v287.4l114.8-120.5c9.3-9.8 24.8-10 34.3-.4z" ] []
                ]

        --, symbol [id "solid-arrow-down",
        SolidArrowUp ->
            symbol [ id <| toString iconType, viewBox "0 0 448 512" ]
                [ path [ d "M34.9 289.5l-22.2-22.2c-9.4-9.4-9.4-24.6 0-33.9L207 39c9.4-9.4 24.6-9.4 33.9 0l194.3 194.3c9.4 9.4 9.4 24.6 0 33.9L413 289.4c-9.5 9.5-25 9.3-34.3-.4L264 168.6V456c0 13.3-10.7 24-24 24h-32c-13.3 0-24-10.7-24-24V168.6L69.2 289.1c-9.3 9.8-24.8 10-34.3.4z" ] []
                ]

        --, symbol [id "solid-arrow-up",
        SolidCrosshairs ->
            symbol [ id <| toString iconType, viewBox "0 0 512 512" ]
                [ path [ d "M500 224h-30.364C455.724 130.325 381.675 56.276 288 42.364V12c0-6.627-5.373-12-12-12h-40c-6.627 0-12 5.373-12 12v30.364C130.325 56.276 56.276 130.325 42.364 224H12c-6.627 0-12 5.373-12 12v40c0 6.627 5.373 12 12 12h30.364C56.276 381.675 130.325 455.724 224 469.636V500c0 6.627 5.373 12 12 12h40c6.627 0 12-5.373 12-12v-30.364C381.675 455.724 455.724 381.675 469.636 288H500c6.627 0 12-5.373 12-12v-40c0-6.627-5.373-12-12-12zM288 404.634V364c0-6.627-5.373-12-12-12h-40c-6.627 0-12 5.373-12 12v40.634C165.826 392.232 119.783 346.243 107.366 288H148c6.627 0 12-5.373 12-12v-40c0-6.627-5.373-12-12-12h-40.634C119.768 165.826 165.757 119.783 224 107.366V148c0 6.627 5.373 12 12 12h40c6.627 0 12-5.373 12-12v-40.634C346.174 119.768 392.217 165.757 404.634 224H364c-6.627 0-12 5.373-12 12v40c0 6.627 5.373 12 12 12h40.634C392.232 346.174 346.243 392.217 288 404.634zM288 256c0 17.673-14.327 32-32 32s-32-14.327-32-32c0-17.673 14.327-32 32-32s32 14.327 32 32z" ] []
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
    , SolidPlusCircle
    , SolidTrash
    , SolidTrashBan
    , SolidQuestionCircle
    , SolidEye
    , SolidEyeSlash
    , SolidArrowUp
    , SolidArrowDown
    , SolidCrosshairs
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
    svg [ style "display: none" ] <|
        List.map iconSymbol all


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
