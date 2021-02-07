module Graph exposing (DataSet, Model, Msg, bringDataSetForward, bringDataSetToFront, hoverDataSet, init, pushDataSetBack, selectDataSet, setFillLines, setShowPoints, toggleDataSet, toggleDataSetSelected, update, viewJustYAxis, viewKey, viewLineGraph)

import Array
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Svg as S exposing (..)
import Svg.Attributes as A exposing (..)
import Svg.Events as E exposing (onClick, onMouseOut, onMouseOver)


type Msg
    = DataPointHovered (Maybe ( Int, Int ))
    | DataPointClicked ( Int, Int )
    | DataLineHovered (Maybe Int)
    | DataLineClicked Int


type alias Model =
    { today : Date
    , data : Dict Int DataSet
    , selectedDataPoint : Maybe ( Int, Int )
    , fillLines : Bool
    , showPoints : Bool
    , selectedDataSet : Maybe Int
    , hoveredDataSet : Maybe Int
    , dataOrder : List Int
    }


type alias DataSet =
    { name : String
    , colour : Colour
    , dataPoints : Dict Int Float
    , visible : Bool
    }



-- INIT


init : Date -> Bool -> Bool -> Dict Int { name : String, colour : Colour, dataPoints : Dict Int Float } -> Model
init today fillLines showPoints dataSets =
    { today = today
    , data =
        dataSets
            |> Dict.map
                (\_ ds ->
                    { name = ds.name
                    , colour = ds.colour
                    , dataPoints = ds.dataPoints
                    , visible = True
                    }
                )
    , selectedDataPoint = Nothing
    , selectedDataSet = Nothing
    , hoveredDataSet = Nothing
    , fillLines = fillLines
    , showPoints = showPoints
    , dataOrder = Dict.keys dataSets
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataPointHovered (Just ( id, d )) ->
            (selectDataPoint (Just ( id, d )) << hoverDataSet (Just id)) model

        DataPointHovered _ ->
            (selectDataPoint Nothing << hoverDataSet Nothing) model

        DataPointClicked ( id, _ ) ->
            toggleDataSetSelected id model

        DataLineHovered id ->
            hoverDataSet id model

        DataLineClicked id ->
            toggleDataSetSelected id model


setFillLines : Bool -> Model -> Model
setFillLines fl model =
    { model | fillLines = fl }


setShowPoints : Bool -> Model -> Model
setShowPoints sp model =
    { model | showPoints = sp }


toggleDataSetSelected : Int -> Model -> Model
toggleDataSetSelected targetId model =
    let
        newSelectedDataSet =
            case model.selectedDataSet of
                Just id ->
                    if id == targetId then
                        Nothing

                    else
                        Just targetId

                _ ->
                    Just targetId
    in
    { model
        | selectedDataSet = newSelectedDataSet
        , hoveredDataSet = Nothing
    }


selectDataPoint : Maybe ( Int, Int ) -> Model -> Model
selectDataPoint p model =
    { model | selectedDataPoint = p }


selectDataSet : Maybe Int -> Model -> Model
selectDataSet id model =
    { model | selectedDataSet = id }


hoverDataSet : Maybe Int -> Model -> Model
hoverDataSet id model =
    { model | hoveredDataSet = id }


bringDataSetToFront : Int -> Model -> Model
bringDataSetToFront id model =
    { model | dataOrder = id :: List.filter (\i -> i /= id) model.dataOrder }


bringDataSetForward : Int -> Model -> Model
bringDataSetForward id model =
    let
        fn ids =
            case ids of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: y :: rest ->
                    if x == id then
                        x :: y :: rest

                    else if y == id then
                        y :: x :: rest

                    else
                        x :: fn (y :: rest)
    in
    { model | dataOrder = fn model.dataOrder }


pushDataSetBack : Int -> Model -> Model
pushDataSetBack id model =
    let
        fn ids =
            case ids of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: y :: rest ->
                    if x == id then
                        y :: x :: rest

                    else
                        x :: fn (y :: rest)
    in
    { model | dataOrder = fn model.dataOrder }


toggleDataSet : Int -> Model -> Model
toggleDataSet id model =
    case Dict.get id model.data of
        Just ds ->
            { model
                | data = Dict.insert id { ds | visible = not ds.visible } model.data
                , selectedDataSet =
                    model.selectedDataSet
                        |> Maybe.andThen
                            (\selectedId ->
                                if selectedId == id && not ds.visible then
                                    Nothing

                                else
                                    Just selectedId
                            )
            }

        _ ->
            model



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


viewJustYAxis : String -> Model -> Svg msg
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
    in
    svg
        [ viewBox w h, A.class class ]
    <|
        line h { axisLine | x1 = w - 1, y1 = minY, x2 = w - 1, y2 = maxY }
            :: List.concatMap
                (\n ->
                    [ line h { axisLine | x1 = v.ml, y1 = minY + toFloat n * v.yStep, x2 = v.ml + v.longDash, y2 = minY + toFloat n * v.yStep }
                    , text_ h { x = v.ml - 5.0, y = minY + toFloat n * v.yStep, dominantBaseline = "middle", textAnchor = "end", text = String.fromInt (n * valueStep) }
                    ]
                )
                (List.range 0 5)


viewLineGraph : String -> Model -> Svg Msg
viewLineGraph class { data, today, selectedDataPoint, selectedDataSet, hoveredDataSet, fillLines, showPoints, dataOrder } =
    let
        startDate =
            findStartDate today data

        maxValue =
            toFloat <| ceiling (findMaxValue data / 5) * 5

        valueStep =
            floor (maxValue / 5)

        startDateRD =
            Date.toRataDie startDate

        dayLength =
            Date.toRataDie (Date.add Days 1 startDate) - startDateRD

        xSteps =
            floor (toFloat (Date.toRataDie today - startDateRD) / toFloat dayLength)

        ( w, h ) =
            ( v.xStep * toFloat xSteps, v.mb + v.mt + v.yStep * 5 )

        minX =
            0

        ( minY, maxY ) =
            ( v.mb, h - v.mt )

        xAxis =
            List.map (\n -> line h { dottedLine | x1 = minX + toFloat n * v.xStep, y1 = minY, x2 = minX + toFloat n * v.xStep, y2 = maxY })
                (List.range 0 xSteps)
                ++ line h { axisLine | x1 = 0, y1 = minY, x2 = w, y2 = minY }
                :: List.concatMap
                    (\n ->
                        if n == 0 then
                            [ line h { axisLine | x1 = minX + toFloat n * v.xStep + 1, y1 = minY, x2 = minX + toFloat n * v.xStep + 1, y2 = minY - v.longDash }
                            , text_ h { x = minX + toFloat n * v.xStep, y = minY - v.longDash - 5.0, dominantBaseline = "hanging", textAnchor = "start", text = Date.format "d MMM" <| Date.add Days n startDate }
                            ]

                        else if n == xSteps then
                            [ line h { axisLine | x1 = minX + toFloat n * v.xStep - 1, y1 = minY, x2 = minX + toFloat n * v.xStep - 1, y2 = minY - v.longDash }
                            , text_ h { x = minX + toFloat n * v.xStep, y = minY - v.longDash - 5.0, dominantBaseline = "hanging", textAnchor = "end", text = Date.format "d MMM" <| Date.add Days n startDate }
                            ]

                        else if xSteps <= 7 || n == xSteps || modBy 7 n == 0 then
                            [ line h { axisLine | x1 = minX + toFloat n * v.xStep, y1 = minY, x2 = minX + toFloat n * v.xStep, y2 = minY - v.longDash }
                            , text_ h { x = minX + toFloat n * v.xStep, y = minY - v.longDash - 5.0, dominantBaseline = "hanging", textAnchor = "middle", text = Date.format "d MMM" <| Date.add Days n startDate }
                            ]

                        else
                            [ line h { axisLine | x1 = minX + toFloat n * v.xStep, y1 = minY, x2 = minX + toFloat n * v.xStep, y2 = minY - v.shortDash } ]
                    )
                    (List.range 0 xSteps)

        yAxis =
            List.map
                (\n -> line h { dottedLine | x1 = 0, y1 = minY + toFloat n * v.yStep, x2 = w, y2 = minY + toFloat n * v.yStep })
                (List.range 1 5)

        axes =
            rect h
                { fillCol = LighterGray
                , x = 0
                , y = minY
                , width = w
                , height = maxY - minY
                }
                :: xAxis
                ++ yAxis

        dataLine : Int -> DataSet -> List (Svg Msg)
        dataLine id dataSet =
            let
                plotPoints : List PlotPoint
                plotPoints =
                    Dict.values <|
                        Dict.map
                            (\date value ->
                                { date = date
                                , value = value
                                , x = minX + toFloat (date - startDateRD) * v.xStep
                                , y = minY + ((value / toFloat valueStep) * v.yStep)
                                }
                            )
                            dataSet.dataPoints
            in
            smoothLinePath h
                { strokeCol = dataSet.colour
                , strokeWidth = 2
                , strokeLinecap = "round"
                , strokeLinejoin = "round"
                , strokeOpacity =
                    if (selectedDataSet == Nothing && hoveredDataSet == Nothing) || selectedDataSet == Just id || hoveredDataSet == Just id then
                        100

                    else
                        30
                , fillCol =
                    if fillLines then
                        Just dataSet.colour

                    else
                        Nothing
                , fillOpacity =
                    if (selectedDataSet == Nothing && hoveredDataSet == Nothing) || selectedDataSet == Just id || hoveredDataSet == Just id then
                        100

                    else
                        30
                , points = List.map (\{ x, y } -> ( x, y )) plotPoints
                , onClick = Just (DataLineClicked id)
                , onMouseOver =
                    case selectedDataSet of
                        Just _ ->
                            Nothing

                        _ ->
                            Just <| DataLineHovered <| Just id
                , onMouseOut =
                    case selectedDataSet of
                        Just _ ->
                            Nothing

                        _ ->
                            Just <| DataLineHovered Nothing
                }
                :: (if showPoints then
                        List.map
                            (\{ date, x, y } ->
                                circle h
                                    { cx = x
                                    , cy = y
                                    , r =
                                        if selectedDataPoint == Just ( id, date ) then
                                            4

                                        else
                                            3
                                    , strokeCol = dataSet.colour
                                    , strokeWidth = 1
                                    , strokeOpacity =
                                        if (selectedDataSet == Nothing && hoveredDataSet == Nothing) || selectedDataSet == Just id || hoveredDataSet == Just id then
                                            100

                                        else if fillLines then
                                            30

                                        else
                                            5
                                    , fillCol = dataSet.colour
                                    , fillOpacity =
                                        if (selectedDataSet == Nothing && hoveredDataSet == Nothing) || selectedDataSet == Just id || hoveredDataSet == Just id then
                                            80

                                        else if fillLines then
                                            50

                                        else
                                            30
                                    , onMouseOver = Just <| DataPointHovered <| Just ( id, date )
                                    , onMouseOut = Just <| DataPointHovered Nothing
                                    , onClick = Just (DataPointClicked ( id, date ))
                                    }
                            )
                            plotPoints

                    else
                        []
                   )

        dataLines =
            Dict.map dataLine <| Dict.filter (\_ ds -> ds.visible) <| data
    in
    svg [ viewBox w h, A.class class ] <|
        (defs [] <|
            List.map
                (\( _, { colour } ) ->
                    linearGradient [ id <| "gradient-" ++ Colour.toString colour, x1 "0", x2 "0", y1 "0", y2 "1" ]
                        [ stop [ offset "0%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "80%" ] []
                        , stop [ offset "100%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "60%" ] []
                        ]
                )
                (Dict.toList data)
        )
            :: axes
            ++ (List.concatMap (\id -> Maybe.withDefault [] <| Dict.get id dataLines) <| List.reverse dataOrder)


viewKey : String -> DataSet -> Svg msg
viewKey class ds =
    let
        ( w, h ) =
            ( 20, 10 )
    in
    svg
        [ viewBox w h
        , A.class class
        ]
        [ rect h
            { fillCol = LighterGray
            , x = 0
            , y = 0
            , width = w
            , height = h
            }
        , line h
            { strokeCol = ds.colour
            , strokeWidth = 2
            , strokeDasharray = Nothing
            , strokeLinecap = "square"
            , x1 = 0
            , y1 = h / 2
            , x2 = w
            , y2 = h / 2
            }
        ]


findStartDate : Date -> Dict Int DataSet -> Date
findStartDate today data =
    let
        minDate =
            Date.fromRataDie
                << Maybe.withDefault (Date.toRataDie today)
                << List.minimum
                << List.filterMap (List.head << Dict.keys << .dataPoints)
                << Dict.values
            <|
                data

        fullWeeks =
            ceiling <| toFloat (Date.diff Days minDate today) / 7
    in
    Date.add Weeks -fullWeeks today


findMaxValue : Dict Int DataSet -> Float
findMaxValue =
    Maybe.withDefault 0
        << List.maximum
        << List.filterMap (List.maximum << Dict.values << .dataPoints)
        << Dict.values


concatMaybes : List (Maybe a) -> List a
concatMaybes maybeXs =
    case maybeXs of
        (Just x) :: xs ->
            x :: concatMaybes xs

        _ :: xs ->
            concatMaybes xs

        _ ->
            []


type alias LineDefn =
    { strokeCol : Colour
    , strokeWidth : Float
    , strokeLinecap : String
    , strokeDasharray : Maybe String
    , x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


axisLine : LineDefn
axisLine =
    { strokeCol = Black, strokeWidth = 1, strokeLinecap = "square", strokeDasharray = Nothing, x1 = 0, y1 = 0, x2 = 0, y2 = 0 }


dottedLine : LineDefn
dottedLine =
    { strokeCol = LightGray
    , strokeWidth = 1
    , strokeDasharray = Just "4"
    , strokeLinecap = "square"
    , x1 = 0
    , y1 = 0
    , x2 = 0
    , y2 = 0
    }


line : Float -> LineDefn -> Svg msg
line h l =
    S.line
        ([ class <| "stroke-" ++ Colour.toString l.strokeCol, strokeLinecap l.strokeLinecap, x1 (String.fromFloat l.x1), y1 (String.fromFloat (h - l.y1)), x2 (String.fromFloat l.x2), y2 (String.fromFloat (h - l.y2)) ]
            ++ (case l.strokeDasharray of
                    Just sda ->
                        [ strokeDasharray sda ]

                    Nothing ->
                        []
               )
        )
        []


type alias RectDefn =
    { fillCol : Colour
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


rect : Float -> RectDefn -> Svg msg
rect h r =
    S.rect [ class <| "fill-" ++ Colour.toString r.fillCol, x (String.fromFloat r.x), y (String.fromFloat (h - r.y - r.height)), width (String.fromFloat r.width), height (String.fromFloat r.height) ] []


type alias CircleDefn msg =
    { cx : Float
    , cy : Float
    , r : Float
    , fillCol : Colour
    , fillOpacity : Int
    , strokeCol : Colour
    , strokeWidth : Float
    , strokeOpacity : Int
    , onMouseOver : Maybe msg
    , onMouseOut : Maybe msg
    , onClick : Maybe msg
    }


circle : Float -> CircleDefn msg -> Svg msg
circle h c =
    S.circle
        ([ cx (String.fromFloat c.cx)
         , cy (String.fromFloat (h - c.cy))
         , r (String.fromFloat c.r)
         , class <| "fill-" ++ Colour.toString c.fillCol
         , fillOpacity <| String.fromInt c.fillOpacity ++ "%"
         , class <| "stroke-" ++ Colour.toString c.strokeCol
         , strokeWidth (String.fromFloat c.strokeWidth)
         , strokeOpacity <| String.fromInt c.strokeOpacity ++ "%"
         ]
            ++ (case c.onMouseOver of
                    Just msg ->
                        [ onMouseOver msg ]

                    _ ->
                        []
               )
            ++ (case c.onMouseOut of
                    Just msg ->
                        [ onMouseOut msg ]

                    _ ->
                        []
               )
            ++ (case c.onClick of
                    Just msg ->
                        [ onClick msg ]

                    _ ->
                        []
               )
        )
        []


type alias TextDefn =
    { x : Float
    , y : Float
    , dominantBaseline : String
    , textAnchor : String
    , text : String
    }


text_ : Float -> TextDefn -> Svg msg
text_ h t =
    S.text_ [ x (String.fromFloat t.x), y (String.fromFloat (h - t.y)), fontSize "10px", dominantBaseline t.dominantBaseline, textAnchor t.textAnchor ] [ text t.text ]


type alias PathDefn msg =
    { strokeCol : Colour
    , strokeWidth : Float
    , strokeLinecap : String
    , strokeLinejoin : String
    , strokeOpacity : Int
    , fillCol : Maybe Colour
    , fillOpacity : Int
    , points : List ( Float, Float )
    , onClick : Maybe msg
    , onMouseOver : Maybe msg
    , onMouseOut : Maybe msg
    }


polyLine : Float -> PathDefn msg -> Svg msg
polyLine h p =
    S.polyline
        ([ fill <| gradient p.fillCol
         , fillOpacity <| String.fromInt p.fillOpacity ++ "%"
         , class <| "stroke-" ++ Colour.toString p.strokeCol
         , strokeWidth (String.fromFloat p.strokeWidth)
         , strokeLinecap p.strokeLinecap
         , strokeLinejoin p.strokeLinejoin
         , strokeOpacity <| String.fromInt p.strokeOpacity ++ "%"
         , points <| String.join " " <| List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat (h - y)) p.points
         ]
            ++ (case p.onClick of
                    Just msg ->
                        [ onClick msg ]

                    _ ->
                        []
               )
            ++ (case p.onMouseOver of
                    Just msg ->
                        [ onMouseOver msg ]

                    _ ->
                        []
               )
            ++ (case p.onMouseOut of
                    Just msg ->
                        [ onMouseOut msg ]

                    _ ->
                        []
               )
        )
        []


straightLinePath : Float -> PathDefn msg -> Svg msg
straightLinePath h p =
    let
        pointsToPath =
            List.indexedMap <|
                \i ( x, y ) ->
                    let
                        x_ =
                            String.fromFloat x

                        y_ =
                            String.fromFloat (h - y)
                    in
                    if i == 0 then
                        "M " ++ x_ ++ "," ++ y_

                    else
                        "L " ++ x_ ++ "," ++ y_
    in
    S.path
        ([ fill <| gradient p.fillCol
         , fillOpacity <| String.fromInt p.fillOpacity ++ "%"
         , class <| "stroke-" ++ Colour.toString p.strokeCol
         , strokeWidth (String.fromFloat p.strokeWidth)
         , strokeLinecap p.strokeLinecap
         , strokeLinejoin p.strokeLinejoin
         , strokeOpacity <| String.fromInt p.strokeOpacity ++ "%"
         , d <| String.join " " <| pointsToPath p.points
         ]
            ++ (case p.onClick of
                    Just msg ->
                        [ onClick msg ]

                    _ ->
                        []
               )
            ++ (case p.onMouseOver of
                    Just msg ->
                        [ onMouseOver msg ]

                    _ ->
                        []
               )
            ++ (case p.onMouseOut of
                    Just msg ->
                        [ onMouseOut msg ]

                    _ ->
                        []
               )
        )
        []


smoothLinePath : Float -> PathDefn msg -> Svg msg
smoothLinePath h p =
    let
        points =
            Array.fromList p.points
    in
    S.path
        ([ class <| "stroke-" ++ Colour.toString p.strokeCol
         , fill <| gradient p.fillCol
         , fillOpacity <| String.fromInt p.fillOpacity ++ "%"
         , strokeWidth (String.fromFloat p.strokeWidth)
         , strokeLinecap p.strokeLinecap
         , strokeLinejoin p.strokeLinejoin
         , strokeOpacity <| String.fromInt p.strokeOpacity ++ "%"
         , d <|
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
                            if i == 0 then
                                case p.fillCol of
                                    Just _ ->
                                        "M " ++ toString ( x, v.mb ) ++ " L " ++ toString ( x, y )

                                    _ ->
                                        "M " ++ toString ( x, y )

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
                                    ++ (case ( p.fillCol, i == Array.length points - 1 ) of
                                            ( Just _, True ) ->
                                                " L "
                                                    ++ toString ( x, v.mb )

                                            _ ->
                                                ""
                                       )
                    )
                    <|
                        points
         ]
            ++ (case p.onClick of
                    Just msg ->
                        [ onClick msg ]

                    _ ->
                        []
               )
            ++ (case p.onMouseOver of
                    Just msg ->
                        [ onMouseOver msg ]

                    _ ->
                        []
               )
            ++ (case p.onMouseOut of
                    Just msg ->
                        [ onMouseOut msg ]

                    _ ->
                        []
               )
        )
        []


gradient : Maybe Colour -> String
gradient maybeCol =
    case maybeCol of
        Just c ->
            "url(#gradient-" ++ Colour.toString c ++ ")"

        _ ->
            "none"


viewBox : Float -> Float -> Attribute msg
viewBox w h =
    A.viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
