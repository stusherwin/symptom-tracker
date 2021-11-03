module Svg.Graph exposing (Model, Msg(..), hoverDataSet, hoverNearestDataPoint, selectDataSet, selectNearestDataPoint, toggleDataSetSelected, toggleDataSetVisible, update, viewJustYAxis, viewLineGraph)

import Array exposing (Array)
import Colour exposing (Colour(..))
import DataSet exposing (DataSet)
import Date exposing (Date, Unit(..))
import Dict
import Extra.Array as Array
import Extra.Html exposing (..)
import Json.Decode as D
import Svg as S exposing (..)
import Svg.Attributes as A exposing (..)
import Svg.Events as E exposing (onMouseOut, onMouseOver)
import Time exposing (Month(..))


type alias Model =
    { today : Date
    , xScale : Float
    , data : Array DataSet
    , selectedDataSet : Maybe Int
    , hoveredDataSet : Maybe Int
    , leavingDataSet : Maybe Int
    , selectedDataPoint : Maybe Int
    , hoveredDataPoint : Maybe Int
    , fillLines : Bool
    , currentWidth : Float
    , minWidth : Float
    , maxWidth : Float
    , height : Float
    }


type Msg
    = DataSetHovered (Maybe Int)
    | DataSetClicked Int
    | MouseDown ( Float, Float )
    | MouseMove ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown ( x, _ ) ->
            let
                xPercent =
                    x / model.maxWidth
            in
            ( model |> selectNearestDataPoint xPercent, Cmd.none )

        MouseMove ( x, _ ) ->
            let
                xPercent =
                    x / model.maxWidth
            in
            ( model |> hoverNearestDataPoint xPercent, Cmd.none )

        DataSetHovered iM ->
            ( model |> hoverDataSet iM, Cmd.none )

        DataSetClicked i ->
            ( model |> toggleDataSetSelected i, Cmd.none )


toggleDataSetSelected : Int -> Model -> Model
toggleDataSetSelected target model =
    let
        ( newSelectedDataSet, leavingDataSet ) =
            case ( model.selectedDataSet, model.selectedDataPoint ) of
                ( Just _, Just _ ) ->
                    ( Just target, Nothing )

                ( Just i, _ ) ->
                    if i == target then
                        ( Nothing, Just i )

                    else
                        ( Just target, Nothing )

                _ ->
                    ( Just target, Nothing )

        wasVisible =
            model.data |> Array.get target |> Maybe.map .isVisible
    in
    case wasVisible of
        Just True ->
            { model
                | selectedDataSet = newSelectedDataSet
                , hoveredDataSet = Nothing
                , leavingDataSet = leavingDataSet
                , selectedDataPoint = Nothing
            }

        _ ->
            model


selectDataSet : Maybe Int -> Model -> Model
selectDataSet targetM model =
    let
        leavingDataSet =
            case ( targetM, model.selectedDataSet ) of
                ( Nothing, Just i ) ->
                    Just i

                _ ->
                    Nothing

        isVisible =
            targetM |> Maybe.andThen (\i -> model.data |> Array.get i |> Maybe.map .isVisible)
    in
    case ( targetM, isVisible ) of
        ( Just i, Just True ) ->
            { model
                | selectedDataSet = Just i
                , hoveredDataSet = Nothing
                , leavingDataSet = leavingDataSet
                , selectedDataPoint = Nothing
            }

        ( Nothing, _ ) ->
            { model
                | selectedDataSet = Nothing
                , hoveredDataSet = Nothing
                , leavingDataSet = leavingDataSet
                , selectedDataPoint = Nothing
            }

        _ ->
            model


hoverDataSet : Maybe Int -> Model -> Model
hoverDataSet targetM model =
    { model
        | hoveredDataSet =
            case targetM of
                Just i ->
                    let
                        isVisible =
                            model.data |> Array.get i |> Maybe.map .isVisible
                    in
                    if model.leavingDataSet == Just i || isVisible /= Just True then
                        Nothing

                    else
                        Just i

                _ ->
                    Nothing
        , leavingDataSet =
            targetM |> Maybe.andThen (always model.leavingDataSet)
    }


selectNearestDataPoint : Float -> Model -> Model
selectNearestDataPoint xPercent model =
    { model
        | selectedDataPoint =
            case model.selectedDataSet of
                Just id ->
                    let
                        nearestPoint =
                            findNearestDataPoint xPercent id model
                    in
                    if model.selectedDataPoint == nearestPoint then
                        Nothing

                    else
                        nearestPoint

                _ ->
                    Nothing
        , hoveredDataPoint = Nothing
    }


hoverNearestDataPoint : Float -> Model -> Model
hoverNearestDataPoint xPercent model =
    { model
        | hoveredDataPoint =
            case ( model.selectedDataPoint, model.selectedDataSet ) of
                ( Nothing, Just i ) ->
                    findNearestDataPoint xPercent i model

                _ ->
                    Nothing
    }


findNearestDataPoint : Float -> Int -> Model -> Maybe Int
findNearestDataPoint xPercent i model =
    case model.data |> Array.get i |> Maybe.map (Dict.keys << .dataPoints) of
        Just dates ->
            let
                startDate =
                    Date.toRataDie <| DataSet.startDate model.today model.data

                range =
                    toFloat <| Date.toRataDie model.today - startDate

                date =
                    toFloat startDate + xPercent * range

                findNearestPoint ds =
                    case ds of
                        [] ->
                            Nothing

                        [ d ] ->
                            Just d

                        d1 :: d2 :: rest ->
                            let
                                d1f =
                                    toFloat d1

                                d2f =
                                    toFloat d2
                            in
                            if d1f <= date && date <= d2f then
                                if date - d1f < d2f - date then
                                    Just d1

                                else
                                    Just d2

                            else
                                findNearestPoint (d2 :: rest)
            in
            findNearestPoint dates

        _ ->
            Nothing


toggleDataSetVisible : Int -> Model -> Model
toggleDataSetVisible i model =
    let
        wasVisible =
            model.data |> Array.get i |> Maybe.map .isVisible
    in
    { model
        | data = model.data |> Array.update i (\ds -> { ds | isVisible = not ds.isVisible })
        , selectedDataSet = Nothing
        , hoveredDataSet = Nothing
        , leavingDataSet =
            case wasVisible of
                Just False ->
                    Just i

                _ ->
                    Nothing
    }



-- VIEW


type alias GraphVals =
    { longDash : Float
    , shortDash : Float
    , xStep : Float
    , yStep : Float
    , mt : Float
    , mb : Float
    , ml : Float
    , mr : Float
    , h : Float
    }


v : GraphVals
v =
    { longDash = 5.0
    , shortDash = 3.0
    , xStep = 15.0
    , yStep = 30.0
    , mt = 10 --20.0
    , mb = 26.0
    , ml = 26.0
    , mr = 0.0
    , h = 186
    }


divideYAxis : Array DataSet -> { maxValue : Float, valueSteps : Float, valueStep : Int, min : Float, max : Float, step : Float }
divideYAxis data =
    let
        ( maxValue, valueSteps ) =
            let
                maxValue_ =
                    DataSet.maxValue data

                log =
                    toFloat <| floor <| logBase 10 maxValue_

                d =
                    10 ^ log
            in
            if maxValue_ <= 5 then
                ( 5, 5 )

            else if maxValue_ <= 3 * d then
                let
                    m =
                        toFloat (ceiling (maxValue_ / (d / 2))) * (d / 2)
                in
                ( m, m / (d / 2) )

            else if maxValue_ <= 5 * d then
                let
                    m =
                        toFloat (ceiling (maxValue_ / d)) * d
                in
                ( m, m / d )

            else
                ( d * 10, 10 )

        valueStep =
            floor (maxValue / valueSteps)

        ( minY, maxY ) =
            ( v.mb, v.h - v.mt )

        yStep =
            (maxY - minY) / valueSteps
    in
    { maxValue = maxValue
    , valueSteps = valueSteps
    , valueStep = valueStep
    , min = minY
    , max = maxY
    , step = yStep
    }


viewJustYAxis : String -> Model -> Svg msg
viewJustYAxis class { data } =
    let
        yDiv =
            divideYAxis data

        ( w, h ) =
            ( v.ml + v.longDash + 1, v.h )

        f_ : (String -> S.Attribute msg) -> Float -> S.Attribute msg
        f_ attr x =
            attr (String.fromFloat x)

        fh_ : (String -> S.Attribute msg) -> Float -> S.Attribute msg
        fh_ attr y =
            attr (String.fromFloat (h - y))
    in
    svg
        [ viewBox w h, A.class class ]
    <|
        axisLine [ f_ x1 (w - 1), fh_ y1 yDiv.min, f_ x2 (w - 1), fh_ y2 yDiv.max ]
            :: List.concatMap
                (\n ->
                    [ axisLine [ f_ x1 v.ml, fh_ y1 <| yDiv.min + toFloat n * yDiv.step, f_ x2 <| v.ml + v.longDash, fh_ y2 <| yDiv.min + toFloat n * yDiv.step ]
                    , text_ [ f_ x <| v.ml - 5.0, fh_ y <| yDiv.min + toFloat n * yDiv.step, fontSize "10px", dominantBaseline "middle", textAnchor "end" ] [ text <| String.fromInt <| n * yDiv.valueStep ]
                    ]
                )
                (List.range 0 <| ceiling yDiv.valueSteps)


viewLineGraph : String -> String -> Model -> Svg Msg
viewLineGraph svgId class m =
    let
        startDate =
            DataSet.startDate m.today m.data

        yDiv =
            divideYAxis m.data

        startDateRD =
            Date.toRataDie startDate

        dayLength =
            Date.toRataDie (Date.add Days 1 startDate) - startDateRD

        xSteps =
            floor (toFloat (Date.toRataDie m.today - startDateRD) / toFloat dayLength)

        xStep =
            if m.currentWidth >= m.minWidth then
                v.xStep * m.xScale

            else
                v.xStep * m.xScale * m.minWidth / m.currentWidth

        ( w, h ) =
            ( if m.height == 0 then
                0

              else
                Basics.max (m.minWidth * v.h / m.height) (xStep * toFloat xSteps)
            , v.h
            )

        minX =
            0

        f_ : (String -> S.Attribute msg) -> Float -> S.Attribute msg
        f_ attr x =
            attr (String.fromFloat x)

        fh_ : (String -> S.Attribute msg) -> Float -> S.Attribute msg
        fh_ attr y =
            attr (String.fromFloat (h - y))

        xAxis =
            axisLine [ f_ x1 0, fh_ y1 yDiv.min, f_ x2 w, fh_ y2 yDiv.min ]
                :: List.concatMap
                    (\n ->
                        let
                            date =
                                Date.add Days n startDate
                        in
                        if n == xSteps then
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep - 0.5, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep - 0.5, fh_ y2 <| yDiv.min - v.longDash ] ]

                        else if xStep < 0.65 then
                            if n == 0 then
                                axisLine [ f_ x1 <| minX + toFloat n * xStep + 0.5, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep + 0.5, fh_ y2 <| yDiv.min - v.longDash ]
                                    :: (if Date.day date > 24 then
                                            []

                                        else
                                            [ text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "y" date ] ]
                                       )

                            else if (xSteps <= 7 || modBy 7 n == 0) && Date.day date < 8 then
                                if Date.month date == Jan then
                                    axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.longDash ]
                                        :: (if n > xSteps - 8 then
                                                []

                                            else
                                                [ text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "y" date ] ]
                                           )

                                else
                                    [ axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.shortDash ]
                                    ]

                            else
                                []

                        else if xStep < 2 then
                            if n == 0 then
                                axisLine [ f_ x1 <| minX + toFloat n * xStep + 0.5, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep + 0.5, fh_ y2 <| yDiv.min - v.longDash ]
                                    :: (if Date.day date > 24 then
                                            []

                                        else
                                            [ text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "y" date ] ]
                                       )

                            else if xSteps <= 7 || modBy 7 n == 0 then
                                if Date.day date < 8 then
                                    axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.longDash ]
                                        :: (if n > xSteps - 8 then
                                                []

                                            else
                                                [ text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ]
                                                    [ text <|
                                                        (if Date.month date == Jan then
                                                            Date.format "y"

                                                         else
                                                            Date.format "MMM"
                                                        )
                                                            date
                                                    ]
                                                ]
                                           )

                                else
                                    [ axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.shortDash ]
                                    ]

                            else
                                []

                        else if xStep < 6 then
                            if n == 0 then
                                axisLine [ f_ x1 <| minX + toFloat n * xStep + 0.5, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep + 0.5, fh_ y2 <| yDiv.min - v.longDash ]
                                    :: (if xStep < 5 && Date.day date > 24 then
                                            []

                                        else
                                            [ text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "MMM y" date ] ]
                                       )

                            else if xSteps <= 7 || modBy 7 n == 0 then
                                if Date.day date < 8 then
                                    axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.longDash ]
                                        :: (if xStep < 5 && n > xSteps - 8 then
                                                []

                                            else
                                                [ text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "MMM y" date ] ]
                                           )

                                else
                                    [ axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.shortDash ]
                                    ]

                            else
                                []

                        else if n == 0 then
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep + 0.5, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep + 0.5, fh_ y2 <| yDiv.min - v.longDash ]
                            , text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "d MMM" date ]
                            ]

                        else if xSteps <= 7 || modBy 7 n == 0 then
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.longDash ]
                            , text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| yDiv.min - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "d MMM" date ]
                            ]

                        else
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 yDiv.min, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| yDiv.min - v.shortDash ] ]
                    )
                    (List.range 0 xSteps)

        yLines =
            List.map
                (\n -> grayLine [ f_ x1 0, fh_ y1 <| yDiv.min + toFloat n * yDiv.step, f_ x2 w, fh_ y2 <| yDiv.min + toFloat n * yDiv.step ])
                (List.range 1 <| ceiling yDiv.valueSteps)

        background =
            rect
                [ fillColour_ LighterGray
                , f_ x 0
                , fh_ y yDiv.max
                , f_ width w
                , f_ height (yDiv.max - yDiv.min)
                ]
                []
                :: yLines

        axes =
            xAxis

        plotPoints : DataSet -> List ( Float, Float )
        plotPoints dataSet =
            Dict.values <|
                Dict.map
                    (\date value ->
                        ( minX + toFloat (date - startDateRD) * xStep
                        , yDiv.min + ((value / toFloat yDiv.valueStep) * yDiv.step)
                        )
                    )
                    dataSet.dataPoints

        dataFill : ( Int, DataSet ) -> List (Svg Msg)
        dataFill ( i, dataSet ) =
            [ S.path
                ([ strokeColour_ dataSet.colour
                 , strokeWidth_ 0
                 , strokeLinecap "round"
                 , strokeLinejoin "round"
                 , fillGradient_ <|
                    case ( m.fillLines, featuredDataSet == Nothing || featuredDataSet == Just i ) of
                        ( True, True ) ->
                            Normal dataSet.colour

                        ( True, False ) ->
                            Normal Colour.Gray

                        ( False, True ) ->
                            Transparent dataSet.colour

                        ( False, False ) ->
                            Transparent Colour.Gray
                 , filter_ <|
                    if featuredDataSet == Nothing || featuredDataSet == Just i then
                        NoFilter

                    else
                        Brighten
                 , dSmoothLine h Closed <| plotPoints dataSet
                 ]
                    ++ (if m.selectedDataSet == Nothing then
                            [ onClickStopPropagation <| DataSetClicked i
                            , onMouseOver <| DataSetHovered <| Just i
                            , onMouseOut <| DataSetHovered Nothing
                            ]

                        else
                            []
                       )
                )
                []
            ]

        dataLineBacking : ( Int, DataSet ) -> List (Svg Msg)
        dataLineBacking ( i, dataSet ) =
            [ S.path
                ([ strokeColour_ <|
                    if featuredDataSet == Nothing || featuredDataSet == Just i then
                        dataSet.colour

                    else
                        Colour.Gray
                 , strokeWidth_ 10
                 , strokeOpacity_ 1
                 , strokeLinecap "round"
                 , strokeLinejoin "round"
                 , filter_ <|
                    if featuredDataSet == Nothing || featuredDataSet == Just i then
                        NoFilter

                    else
                        Brighten
                 , fill "none"
                 , dSmoothLine h Open <| plotPoints dataSet
                 ]
                    ++ (if m.selectedDataSet == Nothing then
                            [ onClickStopPropagation <| DataSetClicked i
                            , onMouseOver <| DataSetHovered <| Just i
                            , onMouseOut <| DataSetHovered Nothing
                            ]

                        else
                            []
                       )
                )
                []
            ]

        dataLine : ( Int, DataSet ) -> List (Svg Msg)
        dataLine ( i, dataSet ) =
            let
                points =
                    plotPoints dataSet
            in
            S.path
                ([ strokeColour_ <|
                    if featuredDataSet == Nothing || featuredDataSet == Just i then
                        dataSet.colour

                    else
                        Colour.Gray
                 , strokeWidth_ 2
                 , strokeLinecap "round"
                 , strokeLinejoin "round"
                 , filter_ <|
                    if featuredDataSet == Nothing || featuredDataSet == Just i then
                        NoFilter

                    else
                        Brighten
                 , fill "none"
                 , dSmoothLine h Open <| points
                 ]
                    ++ (if m.selectedDataSet == Nothing then
                            [ onClickStopPropagation <| DataSetClicked i
                            , onMouseOver <| DataSetHovered <| Just i
                            , onMouseOut <| DataSetHovered Nothing
                            ]

                        else
                            []
                       )
                )
                []
                :: (if m.selectedDataSet == Just i then
                        List.map
                            (\( x, y ) ->
                                circle
                                    [ f_ cx x
                                    , fh_ cy y
                                    , f_ r 3
                                    , strokeColour_ dataSet.colour
                                    , strokeWidth_ 0
                                    , fillColour_ dataSet.colour
                                    ]
                                    []
                            )
                            points

                    else
                        []
                   )

        featuredDataPoint =
            case m.selectedDataPoint of
                Just i ->
                    Just i

                Nothing ->
                    m.hoveredDataPoint

        featuredDataSet =
            case m.selectedDataSet of
                Just i ->
                    Just i

                Nothing ->
                    m.hoveredDataSet

        highlightedDataPoint =
            case ( m.selectedDataSet, featuredDataPoint ) of
                ( Just i, Just date ) ->
                    case m.data |> Array.get i |> Maybe.andThen (.dataPoints >> Dict.get date) of
                        Just value ->
                            let
                                x =
                                    minX + toFloat (date - startDateRD) * xStep

                                y =
                                    yDiv.min + ((value / toFloat yDiv.valueStep) * yDiv.step)
                            in
                            [ highlightLine [ strokeOpacity_ 60, f_ x1 x, fh_ y1 <| yDiv.min + 2, f_ x2 x, fh_ y2 <| y - 1.5 ]
                            , circle
                                [ f_ cx x
                                , fh_ cy y
                                , f_ r 4
                                , strokeColour_ Colour.White
                                , strokeWidth_ 0
                                , fillColour_ Colour.White
                                , fillOpacity_ 60
                                ]
                                []
                            , axisLine [ f_ x1 x, fh_ y1 <| yDiv.min - 3, f_ x2 x, fh_ y2 <| y - 1.5 ]
                            , circle
                                [ f_ cx x
                                , fh_ cy y
                                , f_ r 3
                                , strokeColour_ Colour.Black
                                , strokeWidth_ 0
                                , fillColour_ Colour.Black
                                ]
                                []
                            ]

                        _ ->
                            []

                _ ->
                    []

        definitions =
            defs [] <|
                S.filter [ id "grayscale" ]
                    [ feColorMatrix [ type_ "saturate", values "0.1" ] []
                    ]
                    :: S.filter [ id "brighten" ]
                        [ feComponentTransfer []
                            [ feFuncR [ type_ "linear", slope "1.2" ]
                                []
                            , feFuncG
                                [ type_ "linear", slope "1.2" ]
                                []
                            , feFuncB
                                [ type_ "linear", slope "1.2" ]
                                []
                            ]
                        ]
                    :: ((Colour.Gray :: (m.data |> Array.toList |> List.map .colour))
                            |> List.concatMap
                                (\colour ->
                                    [ linearGradient [ id <| "gradient-opaque-" ++ Colour.toString colour, x1 "0", x2 "0", y1 "0", y2 "1" ]
                                        [ stop [ offset "0%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "90%" ] []
                                        , stop [ offset "100%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "70%" ] []
                                        ]
                                    , linearGradient [ id <| "gradient-normal-" ++ Colour.toString colour, x1 "0", x2 "0", y1 "0", y2 "1" ]
                                        [ stop [ offset "0%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "80%" ] []
                                        , stop [ offset "100%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "60%" ] []
                                        ]
                                    , linearGradient [ id <| "gradient-transparent-" ++ Colour.toString colour, x1 "0", x2 "0", y1 "0", y2 "1" ]
                                        [ stop [ offset "0%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "20%" ] []
                                        , stop [ offset "100%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "0%" ] []
                                        ]
                                    ]
                                )
                       )

        visibleDataSets =
            m.data |> Array.indexedMap Tuple.pair |> Array.toList |> List.filter (.isVisible << Tuple.second)

        selectedDataSet =
            visibleDataSets |> List.filter ((\i -> m.selectedDataSet == Just i) << Tuple.first)

        unselectedDataSets =
            visibleDataSets |> List.filter ((\i -> m.selectedDataSet /= Just i) << Tuple.first)
    in
    svg
        ([ viewBox (w + v.mr) h
         , A.class class
         , A.id svgId
         ]
            ++ (case m.selectedDataSet of
                    Just _ ->
                        let
                            decode =
                                D.map2 Tuple.pair (D.field "offsetX" D.float) (D.field "offsetY" D.float)
                        in
                        [ E.on "mousedown" <| D.map MouseDown decode
                        , E.on "mousemove" <| D.map MouseMove decode
                        ]

                    _ ->
                        []
               )
        )
    <|
        definitions
            :: background
            ++ (case ( m.selectedDataSet, m.fillLines ) of
                    ( Nothing, False ) ->
                        axes
                            ++ (visibleDataSets |> List.concatMap dataFill)
                            ++ (visibleDataSets |> List.concatMap dataLineBacking)
                            ++ (visibleDataSets |> List.concatMap dataLine)

                    ( Nothing, True ) ->
                        axes
                            ++ (visibleDataSets |> List.concatMap (\l -> dataFill l ++ dataLine l))

                    _ ->
                        (unselectedDataSets |> List.concatMap (\l -> dataFill l ++ dataLine l))
                            ++ axes
                            ++ (selectedDataSet |> List.concatMap (\l -> dataFill l ++ dataLine l))
                            ++ highlightedDataPoint
               )


axisLine : List (S.Attribute msg) -> S.Svg msg
axisLine attrs =
    line ([ strokeColour_ Black, strokeWidth_ 1, strokeLinecap "square" ] ++ attrs) []


highlightLine : List (S.Attribute msg) -> S.Svg msg
highlightLine attrs =
    line ([ strokeColour_ White, strokeWidth_ 3, strokeLinecap "square" ] ++ attrs) []


grayLine : List (S.Attribute msg) -> S.Svg msg
grayLine attrs =
    line ([ strokeColour_ LightGray, strokeWidth_ 1, strokeLinecap "square" ] ++ attrs) []


strokeColour_ : Colour -> S.Attribute msg
strokeColour_ col =
    class <| "stroke-" ++ Colour.toString col


strokeWidth_ : Float -> S.Attribute msg
strokeWidth_ w =
    strokeWidth <| String.fromFloat w


strokeOpacity_ : Int -> S.Attribute msg
strokeOpacity_ o =
    strokeOpacity <| String.fromInt o ++ "%"


fillOpacity_ : Int -> S.Attribute msg
fillOpacity_ o =
    fillOpacity <| String.fromInt o ++ "%"


fillColour_ : Colour -> S.Attribute msg
fillColour_ col =
    class <| "fill-" ++ Colour.toString col


points_ : Float -> List ( Float, Float ) -> S.Attribute msg
points_ h pts =
    points <| String.join " " <| List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (h - y)) pts


type LineType
    = Closed
    | Open


dStraightLine : Float -> LineType -> List ( Float, Float ) -> S.Attribute msg
dStraightLine h lineType pts =
    let
        toString ( x_, y_ ) =
            String.fromFloat x_ ++ "," ++ String.fromFloat (h - y_)

        pointsToPath =
            List.indexedMap <|
                \i ( x, y ) ->
                    if i == 0 then
                        case lineType of
                            Open ->
                                "M " ++ toString ( x, y )

                            Closed ->
                                "M " ++ toString ( x, v.mb ) ++ " L " ++ toString ( x, y )

                    else
                        " L "
                            ++ toString ( x, y )
                            ++ (case ( lineType, i == List.length pts - 1 ) of
                                    ( Open, _ ) ->
                                        ""

                                    ( Closed, True ) ->
                                        " L "
                                            ++ toString ( x, v.mb )

                                    _ ->
                                        ""
                               )
    in
    d <| String.join " " <| pointsToPath pts


dSmoothLine : Float -> LineType -> List ( Float, Float ) -> S.Attribute msg
dSmoothLine h lineType pts =
    let
        points =
            Array.fromList pts

        line_ ( x1, y1 ) ( x2, y2 ) =
            let
                lengthX =
                    x2 - x1

                lengthY =
                    y2 - y1
            in
            { length = sqrt <| (lengthX ^ 2) + (lengthY ^ 2)
            , angle = atan2 lengthY lengthX
            }

        controlPoint ( x_, y_ ) prev_ next_ reverse =
            let
                smoothing =
                    0.1

                opposed =
                    line_ prev_ next_

                toHere =
                    line_ prev_ ( x_, y_ )

                fromHere =
                    line_ ( x_, y_ ) next_

                angle =
                    opposed.angle
                        + (if reverse then
                            pi

                           else
                            0
                          )

                length =
                    Basics.min toHere.length fromHere.length * smoothing
            in
            ( x_ + cos angle * length, y_ + sin angle * length )

        toString ( x_, y_ ) =
            String.fromFloat x_ ++ "," ++ String.fromFloat (h - y_)
    in
    d <|
        String.join " " <|
            Array.toList <|
                (Array.indexedMap <|
                    \i ( x, y ) ->
                        let
                            prev2 =
                                Maybe.withDefault ( x, y ) <| Array.get (i - 2) points

                            prev1 =
                                Maybe.withDefault ( x, y ) <| Array.get (i - 1) points

                            next =
                                Maybe.withDefault ( x, y ) <| Array.get (i + 1) points
                        in
                        if i == 0 then
                            case lineType of
                                Open ->
                                    "M " ++ toString ( x, y )

                                Closed ->
                                    "M " ++ toString ( x, v.mb ) ++ " L " ++ toString ( x, y )

                        else
                            let
                                start =
                                    controlPoint prev1 prev2 ( x, y ) False

                                end =
                                    controlPoint ( x, y ) prev1 next True
                            in
                            "C "
                                ++ toString start
                                ++ " "
                                ++ toString end
                                ++ " "
                                ++ toString ( x, y )
                                ++ (case ( lineType, i == Array.length points - 1 ) of
                                        ( Open, _ ) ->
                                            ""

                                        ( Closed, True ) ->
                                            " L "
                                                ++ toString ( x, v.mb )

                                        _ ->
                                            ""
                                   )
                )
                <|
                    points


type Gradient
    = Opaque Colour
    | Normal Colour
    | Transparent Colour


fillGradient_ : Gradient -> S.Attribute msg
fillGradient_ grad =
    fill <|
        case grad of
            Opaque c ->
                "url(#gradient-opaque-" ++ Colour.toString c ++ ")"

            Normal c ->
                "url(#gradient-normal-" ++ Colour.toString c ++ ")"

            Transparent c ->
                "url(#gradient-transparent-" ++ Colour.toString c ++ ")"


type Filter
    = NoFilter
    | Grayscale
    | Brighten


filter_ : Filter -> S.Attribute msg
filter_ f =
    A.filter <|
        case f of
            Grayscale ->
                "url(#grayscale) url(#brighten)"

            -- Brighten ->
            --     "url(#brighten)"
            _ ->
                "none"


viewBox : Float -> Float -> Attribute msg
viewBox w h =
    A.viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
