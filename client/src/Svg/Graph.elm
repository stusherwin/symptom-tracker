module Svg.Graph exposing (DataSet, Model, Msg, hoverDataSet, selectDataSet, toggleDataSetSelected, toggleDataSetVisible, update, viewJustYAxis, viewKey, viewLineGraph)

import Array
import Colour exposing (Colour(..))
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Htmlx
import IdDict exposing (IdDict)
import Listx
import Svg as S exposing (..)
import Svg.Attributes as A exposing (..)
import Svg.Events as E exposing (onClick, onMouseOut, onMouseOver)


type Msg dataSetId
    = DataPointHovered (Maybe ( dataSetId, Int ))
    | DataPointClicked ( dataSetId, Int )
    | DataLineHovered (Maybe dataSetId)
    | DataLineClicked dataSetId


type alias Model m dataSetId dataSet =
    { m
        | today : Date
        , data : List ( dataSetId, DataSet dataSet )
        , selectedDataPoint : Maybe ( dataSetId, Int )
        , hoveredDataPoint : Maybe ( dataSetId, Int )
        , fillLines : Bool
        , showPoints : Bool
        , selectedDataSet : Maybe dataSetId
        , hoveredDataSet : Maybe dataSetId
        , leavingDataSet : Maybe dataSetId
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
        DataPointHovered p ->
            hoverDataPoint p model

        DataPointClicked ( id, d ) ->
            toggleDataPointSelected ( id, d ) model

        DataLineHovered id ->
            hoverDataSet id model

        DataLineClicked id ->
            toggleDataSetSelected id model


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
            case idM of
                Nothing ->
                    Nothing

                _ ->
                    model.leavingDataSet
    }


toggleDataPointSelected : ( dataSetId, Int ) -> Model m dataSetId dataSet -> Model m dataSetId dataSet
toggleDataPointSelected ( id, date ) model =
    let
        ( newSelectedDataPoint, newSelectedDataSet, leavingDataSet ) =
            case model.selectedDataPoint of
                Just p ->
                    if p == ( id, date ) then
                        ( Nothing, Nothing, Just id )

                    else
                        ( Just ( id, date ), Just id, Nothing )

                _ ->
                    ( Just ( id, date ), Just id, Nothing )
    in
    { model
        | selectedDataPoint = newSelectedDataPoint
        , hoveredDataPoint = Nothing
        , selectedDataSet = newSelectedDataSet
        , hoveredDataSet = Nothing
        , leavingDataSet = leavingDataSet
    }


hoverDataPoint : Maybe ( dataSetId, Int ) -> Model m dataSetId dataSet -> Model m dataSetId dataSet
hoverDataPoint p model =
    { model
        | hoveredDataPoint = p
        , hoveredDataSet =
            case p of
                Just ( id, _ ) ->
                    Just id

                _ ->
                    Nothing
    }


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


viewLineGraph : String -> Model m dataSetId dataSet -> Svg (Msg dataSetId)
viewLineGraph class { data, today, selectedDataPoint, hoveredDataPoint, selectedDataSet, hoveredDataSet, fillLines, showPoints } =
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
            line h { axisLine | x1 = 0, y1 = minY, x2 = w, y2 = minY }
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

        xLines =
            List.map (\n -> line h { dottedLine | x1 = minX + toFloat n * v.xStep, y1 = minY, x2 = minX + toFloat n * v.xStep, y2 = maxY })
                (List.range 0 xSteps)

        yLines =
            List.map
                (\n -> line h { dottedLine | x1 = 0, y1 = minY + toFloat n * v.yStep, x2 = w, y2 = minY + toFloat n * v.yStep })
                (List.range 1 5)

        background =
            rect h
                { fillCol = LighterGray
                , x = 0
                , y = minY
                , width = w
                , height = maxY - minY
                }
                :: xLines
                ++ yLines

        axes =
            xAxis

        featuredDataSet =
            case selectedDataSet of
                Just id ->
                    Just id

                Nothing ->
                    hoveredDataSet

        featuredDataPoint =
            case selectedDataPoint of
                Just id ->
                    Just id

                Nothing ->
                    hoveredDataPoint

        dataLine : ( Order, ( dataSetId, DataSet dataSet ) ) -> List (Svg (Msg dataSetId))
        dataLine ( order, ( id, dataSet ) ) =
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
            (if fillLines then
                smoothLinePath h
                    { strokeCol = dataSet.colour
                    , strokeWidth = 0
                    , strokeLinecap = "round"
                    , strokeLinejoin = "round"
                    , strokeOpacity = 0
                    , fillCol = Normal dataSet.colour

                    -- case order of
                    --     EQ ->
                    --         Opaque dataSet.colour
                    --     LT ->
                    --         Normal dataSet.colour
                    --     GT ->
                    --         Transparent dataSet.colour
                    , filter =
                        --NoFilter
                        if (selectedDataSet == Nothing && hoveredDataSet == Nothing) || selectedDataSet == Just id || hoveredDataSet == Just id then
                            -- if featuredDataSet == Nothing || featuredDataSet == Just id then
                            NoFilter

                        else
                            Grayscale
                    , fillOpacity = 100
                    , points = List.map (\{ x, y } -> ( x, y )) plotPoints
                    , onClick = Just (DataLineClicked id)
                    , onMouseOver = Just <| DataLineHovered <| Just id
                    , onMouseOut = Just <| DataLineHovered Nothing
                    }

             else
                S.path [] []
            )
                :: smoothLinePath h
                    { strokeCol = dataSet.colour
                    , strokeWidth = 2
                    , strokeLinecap = "round"
                    , strokeLinejoin = "round"
                    , strokeOpacity = 100

                    -- if order == GT then
                    --     30
                    -- else
                    --     100
                    , fillCol =
                        -- if fillLines then
                        --     Opaque dataSet.colour
                        -- else
                        NoFill
                    , filter =
                        --NoFilter
                        if (selectedDataSet == Nothing && hoveredDataSet == Nothing) || selectedDataSet == Just id || hoveredDataSet == Just id then
                            -- if featuredDataSet == Nothing || featuredDataSet == Just id then
                            NoFilter

                        else
                            Grayscale
                    , fillOpacity = 100
                    , points = List.map (\{ x, y } -> ( x, y )) plotPoints
                    , onClick = Just (DataLineClicked id)
                    , onMouseOver = Just <| DataLineHovered <| Just id
                    , onMouseOut = Just <| DataLineHovered Nothing
                    }
                :: (if showPoints then
                        List.map
                            (\{ date, x, y } ->
                                circle h
                                    { cx = x
                                    , cy = y
                                    , r = 3
                                    , strokeCol = dataSet.colour
                                    , strokeWidth = 0
                                    , strokeOpacity = 100
                                    , fillCol = dataSet.colour
                                    , filter =
                                        -- NoFilter
                                        if (selectedDataSet == Nothing && hoveredDataSet == Nothing) || selectedDataSet == Just id || hoveredDataSet == Just id then
                                            -- if featuredDataSet == Nothing || featuredDataSet == Just id then
                                            NoFilter

                                        else
                                            Grayscale
                                    , fillOpacity = 100

                                    -- if order == GT then
                                    --     50
                                    -- else
                                    --     100
                                    , onMouseOver = Just <| DataPointHovered <| Just ( id, date )
                                    , onMouseOut = Just <| DataPointHovered Nothing
                                    , onClick = Just (DataPointClicked ( id, date ))
                                    }
                            )
                            plotPoints

                    else
                        []
                   )
    in
    svg [ viewBox w h, A.class class ] <|
        (defs [] <|
            S.filter [ id "grayscale" ]
                [ feColorMatrix [ type_ "saturate", values "0.1" ] []
                ]
                :: List.concatMap
                    (\( _, { colour } ) ->
                        [ linearGradient [ id <| "gradient-opaque-" ++ Colour.toString colour, x1 "0", x2 "0", y1 "0", y2 "1" ]
                            [ stop [ offset "0%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "90%" ] []
                            , stop [ offset "100%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "70%" ] []
                            ]
                        , linearGradient [ id <| "gradient-normal-" ++ Colour.toString colour, x1 "0", x2 "0", y1 "0", y2 "1" ]
                            [ stop [ offset "0%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "80%" ] []
                            , stop [ offset "100%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "60%" ] []
                            ]
                        , linearGradient [ id <| "gradient-transparent-" ++ Colour.toString colour, x1 "0", x2 "0", y1 "0", y2 "1" ]
                            [ stop [ offset "0%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "40%" ] []
                            , stop [ offset "100%", A.class <| "stop-" ++ Colour.toString colour, stopOpacity "20%" ] []
                            ]
                        ]
                    )
                    data
        )
            :: background
            ++ ((case featuredDataPoint of
                    Nothing ->
                        axes

                    _ ->
                        []
                )
                    ++ (let
                            data_ =
                                data
                                    |> List.filter (.visible << Tuple.second)
                                    |> List.foldl
                                        (\( id, d ) t ->
                                            if hoveredDataSet == Just id then
                                                ( EQ, ( id, d ) ) :: t

                                            else
                                                case t of
                                                    ( EQ, x ) :: xs ->
                                                        ( GT, ( id, d ) ) :: ( EQ, x ) :: xs

                                                    ( o, x ) :: xs ->
                                                        ( o, ( id, d ) ) :: ( o, x ) :: xs

                                                    [] ->
                                                        [ ( LT, ( id, d ) ) ]
                                        )
                                        []
                                    |> List.reverse
                        in
                        (data_
                            |> List.filter ((\id -> selectedDataSet /= Just id) << Tuple.first << Tuple.second)
                            |> List.concatMap dataLine
                        )
                            ++ (data_
                                    |> List.filter ((\id -> selectedDataSet == Just id) << Tuple.first << Tuple.second)
                                    |> List.concatMap dataLine
                               )
                       )
                    ++ (case featuredDataPoint of
                            Just ( id, date ) ->
                                case data |> Listx.lookup id |> Maybe.andThen (.dataPoints >> Dict.get date) of
                                    Just value ->
                                        let
                                            x =
                                                minX + toFloat (date - startDateRD) * v.xStep

                                            y =
                                                minY + ((value / toFloat valueStep) * v.yStep)
                                        in
                                        [ line h { highlightLine | strokeOpacity = 60, x1 = x, y1 = minY + 1, x2 = x, y2 = y - 1.5 }
                                        , circle h
                                            { cx = x
                                            , cy = y
                                            , r = 4
                                            , strokeCol = Colour.White
                                            , strokeWidth = 0
                                            , strokeOpacity = 0
                                            , fillCol = Colour.White
                                            , filter = NoFilter
                                            , fillOpacity = 60
                                            , onMouseOver = Just <| DataPointHovered <| Just ( id, date )
                                            , onMouseOut = Just <| DataPointHovered Nothing
                                            , onClick = Just (DataPointClicked ( id, date ))
                                            }
                                        ]
                                            ++ axes
                                            ++ [ line h { axisLine | x1 = x, y1 = minY - 3, x2 = x, y2 = y - 1.5 }
                                               , circle h
                                                    { cx = x
                                                    , cy = y
                                                    , r = 3
                                                    , strokeCol = Colour.Black
                                                    , strokeWidth = 0
                                                    , strokeOpacity = 100
                                                    , fillCol = Colour.Black
                                                    , fillOpacity = 100
                                                    , filter = NoFilter
                                                    , onMouseOver = Just <| DataPointHovered <| Just ( id, date )
                                                    , onMouseOut = Just <| DataPointHovered Nothing
                                                    , onClick = Just (DataPointClicked ( id, date ))
                                                    }
                                               ]

                                    _ ->
                                        []

                            _ ->
                                []
                       )
               )


viewKey : String -> Colour -> Svg msg
viewKey class colour =
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
            { strokeCol = colour
            , strokeWidth = 2
            , strokeDasharray = Nothing
            , strokeLinecap = "square"
            , strokeOpacity = 100
            , filter = NoFilter
            , x1 = 0
            , y1 = h / 2
            , x2 = w
            , y2 = h / 2
            }
        ]


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


type alias LineDefn =
    { strokeCol : Colour
    , strokeWidth : Float
    , strokeLinecap : String
    , strokeDasharray : Maybe String
    , strokeOpacity : Int
    , filter : Filter
    , x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


axisLine : LineDefn
axisLine =
    { strokeCol = Black, filter = NoFilter, strokeWidth = 1, strokeLinecap = "square", strokeOpacity = 100, strokeDasharray = Nothing, x1 = 0, y1 = 0, x2 = 0, y2 = 0 }


highlightLine : LineDefn
highlightLine =
    { strokeCol = White, filter = NoFilter, strokeWidth = 3, strokeOpacity = 100, strokeLinecap = "square", strokeDasharray = Nothing, x1 = 0, y1 = 0, x2 = 0, y2 = 0 }


dottedLine : LineDefn
dottedLine =
    { strokeCol = LightGray
    , strokeWidth = 1
    , strokeDasharray = Just "4"
    , strokeLinecap = "square"
    , strokeOpacity = 100
    , filter = NoFilter
    , x1 = 0
    , y1 = 0
    , x2 = 0
    , y2 = 0
    }


line : Float -> LineDefn -> Svg msg
line h l =
    S.line
        ([ class <| "stroke-" ++ Colour.toString l.strokeCol
         , A.filter <| filter l.filter
         , strokeWidth <| String.fromFloat l.strokeWidth
         , strokeOpacity <| String.fromInt l.strokeOpacity ++ "%"
         , strokeLinecap l.strokeLinecap
         , x1 (String.fromFloat l.x1)
         , y1 (String.fromFloat (h - l.y1))
         , x2 (String.fromFloat l.x2)
         , y2 (String.fromFloat (h - l.y2))
         ]
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
    S.rect
        [ class <| "fill-" ++ Colour.toString r.fillCol
        , x (String.fromFloat r.x)
        , y (String.fromFloat (h - r.y - r.height))
        , width (String.fromFloat r.width)
        , height (String.fromFloat r.height)
        ]
        []


type alias CircleDefn msg =
    { cx : Float
    , cy : Float
    , r : Float
    , fillCol : Colour
    , fillOpacity : Int
    , filter : Filter
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
         , A.filter <| filter c.filter
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
                        [ Htmlx.onClickStopPropagation msg ]

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
    , fillCol : FillColour
    , fillOpacity : Int
    , filter : Filter
    , points : List ( Float, Float )
    , onClick : Maybe msg
    , onMouseOver : Maybe msg
    , onMouseOut : Maybe msg
    }


polyLine : Float -> PathDefn msg -> Svg msg
polyLine h p =
    S.polyline
        ([ fill <| gradient p.fillCol
         , A.filter <| filter p.filter
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
                        [ Htmlx.onClickStopPropagation msg ]

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
         , A.filter <| filter p.filter
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
                        [ Htmlx.onClickStopPropagation msg ]

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
         , A.filter <| filter p.filter
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
                                    NoFill ->
                                        "M " ++ toString ( x, y )

                                    _ ->
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
                                    ++ (case ( p.fillCol, i == Array.length points - 1 ) of
                                            ( NoFill, _ ) ->
                                                ""

                                            ( _, True ) ->
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
                        [ Htmlx.onClickStopPropagation msg ]

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


type FillColour
    = NoFill
    | Opaque Colour
    | Normal Colour
    | Transparent Colour


type Filter
    = NoFilter
    | Grayscale


gradient : FillColour -> String
gradient maybeCol =
    case maybeCol of
        Opaque c ->
            "url(#gradient-opaque-" ++ Colour.toString c ++ ")"

        Normal c ->
            "url(#gradient-normal-" ++ Colour.toString c ++ ")"

        Transparent c ->
            "url(#gradient-transparent-" ++ Colour.toString c ++ ")"

        _ ->
            "none"


filter : Filter -> String
filter f =
    case f of
        Grayscale ->
            "url(#grayscale)"

        _ ->
            ""


viewBox : Float -> Float -> Attribute msg
viewBox w h =
    A.viewBox ("0 0 " ++ String.fromFloat w ++ " " ++ String.fromFloat h)
