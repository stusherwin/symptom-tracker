module Svg.Graph exposing (DataSet, Model, Msg(..), hoverDataSet, hoverNearestDataPoint, selectDataSet, selectNearestDataPoint, toggleDataSetSelected, toggleDataSetVisible, update, viewJustYAxis, viewLineGraph)

import Array
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Htmlx
import IdDict exposing (IdDict)
import Json.Decode as D
import Listx
import Svg as S exposing (..)
import Svg.Attributes as A exposing (..)
import Svg.Events as E exposing (onClick, onMouseOut, onMouseOver)


type Msg dataSetId
    = DataLineHovered (Maybe dataSetId)
    | DataLineClicked dataSetId
    | MouseDown ( Float, Float )
    | MouseMove ( Float, Float )


type alias Model m dataSetId dataSet =
    { m
        | today : Date
        , xScale : Float
        , data : List ( dataSetId, DataSet dataSet )
        , selectedDataSet : Maybe dataSetId
        , hoveredDataSet : Maybe dataSetId
        , leavingDataSet : Maybe dataSetId
        , selectedDataPoint : Maybe Int
        , hoveredDataPoint : Maybe Int
        , fillLines : Bool
        , currentWidth : Float
        , minWidth : Float
    }


type alias DataSet dataSet =
    { dataSet
        | name : String
        , colour : Colour
        , dataPoints : Dict Int Float
        , visible : Bool
    }



-- UPDATE


update : Msg dataSetId -> Model m dataSetId dataSet -> Model m dataSetId dataSet
update msg model =
    case msg of
        DataLineHovered id ->
            hoverDataSet id model

        DataLineClicked id ->
            toggleDataSetSelected id model

        _ ->
            model


toggleDataSetSelected : dataSetId -> Model m dataSetId dataSet -> Model m dataSetId dataSet
toggleDataSetSelected targetId model =
    let
        ( newSelectedDataSet, leavingDataSet ) =
            case ( model.selectedDataSet, model.selectedDataPoint ) of
                ( Just _, Just _ ) ->
                    ( Just targetId, Nothing )

                ( Just id, _ ) ->
                    if id == targetId then
                        ( Nothing, Just id )

                    else
                        ( Just targetId, Nothing )

                _ ->
                    ( Just targetId, Nothing )

        wasVisible =
            model.data |> Listx.lookup targetId |> Maybe.map .visible
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


selectDataSet : Maybe dataSetId -> Model m dataSetId dataSet -> Model m dataSetId dataSet
selectDataSet targetId model =
    let
        leavingDataSet =
            case ( targetId, model.selectedDataSet ) of
                ( Nothing, Just id ) ->
                    Just id

                _ ->
                    Nothing

        visible =
            targetId |> Maybe.andThen (\id -> model.data |> Listx.lookup id |> Maybe.map .visible)
    in
    case ( targetId, visible ) of
        ( Just id, Just True ) ->
            { model
                | selectedDataSet = Just id
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


hoverDataSet : Maybe dataSetId -> Model m dataSetId dataSet -> Model m dataSetId dataSet
hoverDataSet idM model =
    { model
        | hoveredDataSet =
            case idM of
                Just id ->
                    let
                        visible =
                            model.data |> Listx.lookup id |> Maybe.map .visible
                    in
                    if model.leavingDataSet == Just id || visible /= Just True then
                        Nothing

                    else
                        Just id

                _ ->
                    Nothing
        , leavingDataSet =
            idM |> Maybe.andThen (always model.leavingDataSet)
    }


selectNearestDataPoint : ( Float, Float ) -> Model m dataSetId dataSet -> Model m dataSetId dataSet
selectNearestDataPoint ( xPerc, yPerc ) model =
    { model
        | selectedDataPoint =
            case model.selectedDataSet of
                Just id ->
                    let
                        nearestPoint =
                            findNearestDataPoint xPerc id model
                    in
                    if model.selectedDataPoint == nearestPoint then
                        Nothing

                    else
                        nearestPoint

                _ ->
                    Nothing
        , hoveredDataPoint = Nothing
    }


hoverNearestDataPoint : ( Float, Float ) -> Model m dataSetId dataSet -> Model m dataSetId dataSet
hoverNearestDataPoint ( xPerc, yPerc ) model =
    { model
        | hoveredDataPoint =
            case ( model.selectedDataPoint, model.selectedDataSet ) of
                ( Nothing, Just id ) ->
                    findNearestDataPoint xPerc id model

                _ ->
                    Nothing
    }


findNearestDataPoint : Float -> dataSetId -> Model m dataSetId dataSet -> Maybe Int
findNearestDataPoint xPerc id model =
    case model.data |> Listx.lookup id |> Maybe.map (Dict.keys << .dataPoints) of
        Just dates ->
            let
                startDate =
                    Date.toRataDie <| findStartDate model.today model.data

                range =
                    toFloat <| Date.toRataDie model.today - startDate

                date =
                    toFloat startDate + xPerc * range

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


toggleDataSetVisible : dataSetId -> Model m dataSetId dataSet -> Model m dataSetId dataSet
toggleDataSetVisible id model =
    let
        wasVisible =
            model.data |> Listx.lookup id |> Maybe.map .visible
    in
    { model
        | data = model.data |> Listx.updateLookup id (\ds -> { ds | visible = not ds.visible })
        , selectedDataSet = Nothing
        , hoveredDataSet = Nothing
        , leavingDataSet =
            case wasVisible of
                Just False ->
                    Just id

                _ ->
                    Nothing
    }



-- VIEW


type alias PlotPoint =
    { date : Int
    , value : Float
    , x : Float
    , y : Float
    }


type alias GraphVals =
    { longDash : Float
    , shortDash : Float
    , xStep : Float
    , yStep : Float
    , mt : Float
    , mb : Float
    , ml : Float
    , mr : Float
    }


v : GraphVals
v =
    { longDash = 5.0
    , shortDash = 3.0
    , xStep = 15.0
    , yStep = 30.0
    , mt = 10 --20.0
    , mb = 26.0
    , ml = 16.0
    , mr = 16.0
    }


viewJustYAxis : String -> Model m dataSetId dataSet -> Svg msg
viewJustYAxis class { data } =
    let
        maxValue =
            toFloat <| ceiling (findMaxValue data / 5) * 5

        valueStep =
            floor (maxValue / 5)

        ( w, h ) =
            ( v.ml + v.longDash + 1, v.mb + v.mt + v.yStep * 5 )

        ( minY, maxY ) =
            ( v.mb, h - v.mt )

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
        axisLine [ f_ x1 (w - 1), fh_ y1 minY, f_ x2 (w - 1), fh_ y2 maxY ]
            :: List.concatMap
                (\n ->
                    [ axisLine [ f_ x1 v.ml, fh_ y1 <| minY + toFloat n * v.yStep, f_ x2 <| v.ml + v.longDash, fh_ y2 <| minY + toFloat n * v.yStep ]
                    , text_ [ f_ x <| v.ml - 5.0, fh_ y <| minY + toFloat n * v.yStep, fontSize "10px", dominantBaseline "middle", textAnchor "end" ] [ text <| String.fromInt (n * valueStep) ]
                    ]
                )
                (List.range 0 5)


viewLineGraph : String -> String -> Model m dataSetId dataSet -> Svg (Msg dataSetId)
viewLineGraph svgId class m =
    let
        startDate =
            findStartDate m.today m.data

        maxValue =
            toFloat <| ceiling (findMaxValue m.data / 5) * 5

        valueStep =
            floor (maxValue / 5)

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
            ( xStep * toFloat xSteps, v.mb + v.mt + v.yStep * 5 )

        minX =
            0

        ( minY, maxY ) =
            ( v.mb, h - v.mt )

        f_ : (String -> S.Attribute msg) -> Float -> S.Attribute msg
        f_ attr x =
            attr (String.fromFloat x)

        fh_ : (String -> S.Attribute msg) -> Float -> S.Attribute msg
        fh_ attr y =
            attr (String.fromFloat (h - y))

        xAxis =
            axisLine [ f_ x1 0, fh_ y1 minY, f_ x2 w, fh_ y2 minY ]
                :: List.concatMap
                    (\n ->
                        if n == 0 then
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep + 1, fh_ y1 minY, f_ x2 <| minX + toFloat n * xStep + 1, fh_ y2 <| minY - v.longDash ]
                            , text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| minY - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "start" ] [ text <| Date.format "d MMM" <| Date.add Days n startDate ]
                            ]

                        else if n == xSteps then
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep - 1, fh_ y1 minY, f_ x2 <| minX + toFloat n * xStep - 1, fh_ y2 <| minY - v.longDash ]
                            , text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| minY - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "end" ] [ text <| Date.format "d MMM" <| Date.add Days n startDate ]
                            ]

                        else if xSteps <= 7 || n == xSteps || modBy 7 n == 0 then
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 minY, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| minY - v.longDash ]
                            , text_ [ f_ x <| minX + toFloat n * xStep, fh_ y <| minY - v.longDash - 5.0, fontSize "10px", dominantBaseline "hanging", textAnchor "middle" ] [ text <| Date.format "d MMM" <| Date.add Days n startDate ]
                            ]

                        else
                            [ axisLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 minY, f_ x2 <| minX + toFloat n * xStep, fh_ y2 <| minY - v.shortDash ] ]
                    )
                    (List.range 0 xSteps)

        xLines =
            List.map (\n -> dottedLine [ f_ x1 <| minX + toFloat n * xStep, fh_ y1 minY, f_ x2 <| minX + toFloat n * xStep, fh_ y2 maxY ])
                (List.range 0 xSteps)

        yLines =
            List.map
                (\n -> dottedLine [ f_ x1 0, fh_ y1 <| minY + toFloat n * v.yStep, f_ x2 w, fh_ y2 <| minY + toFloat n * v.yStep ])
                (List.range 1 5)

        background =
            rect
                [ fillColour_ LighterGray
                , f_ x 0
                , fh_ y maxY
                , f_ width w
                , f_ height (maxY - minY)
                ]
                []
                :: xLines
                ++ yLines

        axes =
            xAxis

        dataLine : ( dataSetId, DataSet dataSet ) -> List (Svg (Msg dataSetId))
        dataLine ( id, dataSet ) =
            let
                plotPoints : List PlotPoint
                plotPoints =
                    Dict.values <|
                        Dict.map
                            (\date value ->
                                { date = date
                                , value = value
                                , x = minX + toFloat (date - startDateRD) * xStep
                                , y = minY + ((value / toFloat valueStep) * v.yStep)
                                }
                            )
                            dataSet.dataPoints
            in
            S.path
                ([ strokeColour_ dataSet.colour
                 , strokeWidth_ 0
                 , strokeLinecap "round"
                 , strokeLinejoin "round"
                 , fillGradient_ <|
                    case ( m.fillLines, (m.selectedDataSet == Nothing && m.hoveredDataSet == Nothing) || m.selectedDataSet == Just id || m.hoveredDataSet == Just id ) of
                        ( True, True ) ->
                            Normal dataSet.colour

                        ( True, False ) ->
                            Normal Colour.Gray

                        ( False, True ) ->
                            Transparent dataSet.colour

                        ( False, False ) ->
                            Transparent Colour.Gray
                 , filter_ <|
                    if (m.selectedDataSet == Nothing && m.hoveredDataSet == Nothing) || m.selectedDataSet == Just id || m.hoveredDataSet == Just id then
                        NoFilter

                    else
                        Brighten
                 , dSmoothLine h Closed <| List.map (\{ x, y } -> ( x, y )) plotPoints
                 ]
                    ++ (if m.selectedDataSet == Nothing then
                            [ Htmlx.onClickStopPropagation <| DataLineClicked id
                            , onMouseOver <| DataLineHovered <| Just id
                            , onMouseOut <| DataLineHovered Nothing
                            ]

                        else
                            []
                       )
                )
                []
                :: S.path
                    ([ strokeColour_ <|
                        if (m.selectedDataSet == Nothing && m.hoveredDataSet == Nothing) || m.selectedDataSet == Just id || m.hoveredDataSet == Just id then
                            dataSet.colour

                        else
                            Colour.Gray
                     , strokeWidth_ 2
                     , strokeLinecap "round"
                     , strokeLinejoin "round"
                     , filter_ <|
                        if (m.selectedDataSet == Nothing && m.hoveredDataSet == Nothing) || m.selectedDataSet == Just id || m.hoveredDataSet == Just id then
                            NoFilter

                        else
                            Brighten
                     , fill "none"
                     , dSmoothLine h Open <| List.map (\{ x, y } -> ( x, y )) plotPoints
                     ]
                        ++ (if m.selectedDataSet == Nothing then
                                [ Htmlx.onClickStopPropagation <| DataLineClicked id
                                , onMouseOver <| DataLineHovered <| Just id
                                , onMouseOut <| DataLineHovered Nothing
                                ]

                            else
                                []
                           )
                    )
                    []
                :: (if m.selectedDataSet == Just id then
                        List.map
                            (\{ x, y } ->
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
                            plotPoints

                    else
                        []
                   )

        highlightedDataPoint =
            let
                featuredDataPoint =
                    case m.selectedDataPoint of
                        Just id ->
                            Just id

                        Nothing ->
                            m.hoveredDataPoint
            in
            case ( m.selectedDataSet, featuredDataPoint ) of
                ( Just id, Just date ) ->
                    case m.data |> Listx.lookup id |> Maybe.andThen (.dataPoints >> Dict.get date) of
                        Just value ->
                            let
                                x =
                                    minX + toFloat (date - startDateRD) * xStep

                                y =
                                    minY + ((value / toFloat valueStep) * v.yStep)
                            in
                            [ highlightLine [ strokeOpacity_ 60, f_ x1 x, fh_ y1 <| minY + 2, f_ x2 x, fh_ y2 <| y - 1.5 ]
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
                            , axisLine [ f_ x1 x, fh_ y1 <| minY - 3, f_ x2 x, fh_ y2 <| y - 1.5 ]
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
                    :: ((Colour.Gray :: (m.data |> List.map (.colour << Tuple.second)))
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
    in
    svg
        ([ viewBox w h
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
            ++ (case m.selectedDataSet of
                    Nothing ->
                        axes

                    _ ->
                        []
               )
            ++ (m.data
                    |> List.filter (.visible << Tuple.second)
                    |> List.filter ((\id -> m.selectedDataSet /= Just id) << Tuple.first)
                    |> List.concatMap dataLine
               )
            ++ (case m.selectedDataSet of
                    Just _ ->
                        axes

                    _ ->
                        []
               )
            ++ (m.data
                    |> List.filter (.visible << Tuple.second)
                    |> List.filter ((\id -> m.selectedDataSet == Just id) << Tuple.first)
                    |> List.concatMap dataLine
               )
            ++ highlightedDataPoint


findStartDate : Date -> List ( dataSetId, DataSet dataSet ) -> Date
findStartDate today data =
    let
        minDate =
            Date.fromRataDie
                << Maybe.withDefault (Date.toRataDie today)
                << List.minimum
                << List.filterMap (List.head << Dict.keys << .dataPoints)
                << List.map Tuple.second
            <|
                data

        fullWeeks =
            ceiling <| toFloat (Date.diff Days minDate today) / 7
    in
    Date.add Weeks -fullWeeks today


findMaxValue : List ( dataSetId, DataSet dataSet ) -> Float
findMaxValue =
    Maybe.withDefault 0
        << List.maximum
        << List.filterMap (List.maximum << Dict.values << .dataPoints)
        << List.map Tuple.second


axisLine : List (S.Attribute msg) -> S.Svg msg
axisLine attrs =
    line ([ strokeColour_ Black, strokeWidth_ 1, strokeLinecap "square" ] ++ attrs) []


highlightLine : List (S.Attribute msg) -> S.Svg msg
highlightLine attrs =
    line ([ strokeColour_ White, strokeWidth_ 3, strokeLinecap "square" ] ++ attrs) []


dottedLine : List (S.Attribute msg) -> S.Svg msg
dottedLine attrs =
    line ([ strokeColour_ LightGray, strokeWidth_ 1, strokeLinecap "square", strokeDasharray "4" ] ++ attrs) []


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
