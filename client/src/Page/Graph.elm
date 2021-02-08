module Page.Graph exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Button
import Chart exposing (Chart(..))
import Date exposing (Date, Unit(..))
import Dict
import Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, onMouseEnter, onMouseLeave, preventDefaultOn, stopPropagationOn)
import Icon exposing (IconType(..), icon)
import Json.Decode as Decode
import Maybe exposing (Maybe)
import Task
import Time exposing (Month(..))
import Trackable exposing (Trackable, TrackableData(..))
import UserData exposing (UserData)


type alias Model =
    { today : Date
    , graph : Maybe Graph.Model
    }


init : Date -> UserData -> ( Model, Cmd Msg )
init today userData =
    ( { today = today
      , graph = initGraph today userData
      }
    , Dom.getViewportOf "chart"
        |> Task.andThen (\info -> Dom.setViewportOf "chart" info.scene.width 0)
        |> Task.attempt (always NoOp)
    )


initGraph : Date -> UserData -> Maybe Graph.Model
initGraph today userData =
    case List.head <| Dict.values (UserData.charts userData) of
        Just (LineChart chart) ->
            Just <|
                Graph.init today
                    chart.fillLines
                    chart.showPoints
                    (chart.chartables
                        |> List.filterMap
                            (\id ->
                                Dict.get id (UserData.chartables userData)
                                    |> Maybe.map (\c -> ( id, c ))
                            )
                        |> Dict.fromList
                        |> Dict.map
                            (\_ c ->
                                { name = c.name
                                , colour = c.colour
                                , dataPoints =
                                    let
                                        dataPoints =
                                            c.sum
                                                |> List.filterMap
                                                    (\( id, multiplier ) ->
                                                        Dict.get id (UserData.trackables userData)
                                                            |> Maybe.map
                                                                (\t ->
                                                                    Dict.map (\d v -> v * multiplier) <| Trackable.onlyFloatData t
                                                                )
                                                    )
                                                |> List.foldl
                                                    (\d1 d2 ->
                                                        Dict.merge
                                                            Dict.insert
                                                            (\d v1 v2 -> Dict.insert d (v1 + v2))
                                                            Dict.insert
                                                            d1
                                                            d2
                                                            Dict.empty
                                                    )
                                                    Dict.empty
                                    in
                                    if c.inverted then
                                        let
                                            maxValue =
                                                List.maximum <| Dict.values dataPoints
                                        in
                                        case maxValue of
                                            Just max ->
                                                dataPoints |> Dict.map (\_ v -> max - v)

                                            _ ->
                                                dataPoints

                                    else
                                        dataPoints
                                }
                            )
                    )
                    chart.chartables

        _ ->
            Nothing



-- UPDATE


type Msg
    = NoOp
    | FillLinesChecked Bool
    | ShowPointsChecked Bool
    | DataSetHovered (Maybe Int)
    | DataSetClicked Int
    | DataSetVisibleClicked Int
    | DataSetBringForwardClicked Int
    | DataSetPushBackClicked Int
    | GraphMsg Graph.Msg
    | UserDataChanged UserData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FillLinesChecked fl ->
            ( { model | graph = model.graph |> Maybe.map (Graph.setFillLines fl) }, Cmd.none )

        ShowPointsChecked sp ->
            ( { model | graph = model.graph |> Maybe.map (Graph.setShowPoints sp) }, Cmd.none )

        DataSetHovered id ->
            ( { model | graph = model.graph |> Maybe.map (Graph.hoverDataSet id) }, Cmd.none )

        DataSetClicked id ->
            ( { model | graph = model.graph |> Maybe.map (Graph.toggleDataSetSelected id) }, Cmd.none )

        DataSetVisibleClicked id ->
            ( { model | graph = model.graph |> Maybe.map (Graph.toggleDataSet id) }, Cmd.none )

        DataSetBringForwardClicked id ->
            ( { model | graph = model.graph |> Maybe.map (Graph.bringDataSetForward id) }, Cmd.none )

        DataSetPushBackClicked id ->
            ( { model | graph = model.graph |> Maybe.map (Graph.pushDataSetBack id) }, Cmd.none )

        GraphMsg graphMsg ->
            ( { model | graph = model.graph |> Maybe.map (Graph.update graphMsg) }, Cmd.none )

        UserDataChanged userData ->
            ( { model | graph = initGraph model.today userData }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "shadow-inner-t-md" ]
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
        , case model.graph of
            Just g ->
                viewChart g

            _ ->
                div [] []
        ]


viewChart : Graph.Model -> Html Msg
viewChart graph =
    let
        dataSets =
            graph.data
                |> Dict.map
                    (\id ds ->
                        let
                            canPushBack =
                                ds.visible && List.head graph.dataOrder /= Just id

                            canBringForward =
                                ds.visible && (List.head << List.reverse) graph.dataOrder /= Just id

                            canSelect =
                                ds.visible
                        in
                        [ div
                            [ class "p-2 flex first:mt-0 items-center"
                            , classList
                                [ ( "bg-gray-300", graph.selectedDataSet == Just id || graph.hoveredDataSet == Just id )
                                ]
                            , onMouseEnter <|
                                case graph.selectedDataSet of
                                    Just _ ->
                                        NoOp

                                    _ ->
                                        DataSetHovered (Just id)
                            , onMouseLeave <|
                                case graph.selectedDataSet of
                                    Just _ ->
                                        NoOp

                                    _ ->
                                        DataSetHovered Nothing
                            , onClick (DataSetClicked id)
                            ]
                            [ div
                                [ class "w-16 h-8 mr-4 flex-grow-0 flex-shrink-0"
                                ]
                                [ Graph.viewKey "w-full h-full" ds
                                ]
                            , span [ class "mr-4" ] [ text ds.name ]
                            , button
                                [ class "ml-auto text-black"
                                , classList
                                    [ ( "text-opacity-30 cursor-default", not canPushBack )
                                    , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canPushBack )
                                    ]
                                , onClickStopPropagation (DataSetPushBackClicked id)
                                , disabled (not canPushBack)
                                ]
                                [ icon "w-6 h-6" <| SolidArrowUp
                                ]
                            , button
                                [ class "ml-2 text-black"
                                , classList
                                    [ ( "text-opacity-30 cursor-default", not canBringForward )
                                    , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canBringForward )
                                    ]
                                , onClickStopPropagation (DataSetBringForwardClicked id)
                                , disabled (not canBringForward)
                                ]
                                [ icon "w-6 h-6" <| SolidArrowDown
                                ]
                            , button
                                [ class "ml-2 text-black"
                                , class "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                , onClickStopPropagation (DataSetVisibleClicked id)
                                ]
                                [ icon "w-6 h-6" <|
                                    if ds.visible then
                                        SolidEye

                                    else
                                        SolidEyeSlash
                                ]
                            ]
                        ]
                    )
    in
    div []
        [ div [ class "mx-4 my-0 flex scrollable-parent", style "height" "300px" ]
            [ viewJustYAxis "flex-grow-0 flex-shrink-0" graph
            , viewScrollableContainer [ Html.map GraphMsg <| viewLineGraph "h-full" graph ]
            ]
        , div [ class "m-4 mt-4 flex flex-wrap justify-end" ]
            [ label [ class "text-right whitespace-nowrap", for "fill-lines" ] [ text "Colour under curves" ]
            , input
                [ type_ "checkbox"
                , id "fill-lines"
                , class "ml-2"
                , onCheck FillLinesChecked
                , checked graph.fillLines
                ]
                []
            , label [ class "ml-8 text-right whitespace-nowrap", for "show-points" ] [ text "Show data points" ]
            , input
                [ type_ "checkbox"
                , id "show-points"
                , class "ml-2"
                , onCheck ShowPointsChecked
                , checked graph.showPoints
                ]
                []
            ]
        , div [ class "m-4 mt-4" ] <|
            (List.concatMap (\id -> Maybe.withDefault [] <| Dict.get id dataSets) <| graph.dataOrder)
        ]


viewScrollableContainer : List (Html msg) -> Html msg
viewScrollableContainer children =
    div [ class "relative flex-grow" ]
        [ node "scrollable-container"
            [ id "chart"
            , class "absolute overflow-x-scroll top-0 bottom-scrollbar"
            ]
            children
        ]


concatMaybes : List (Maybe a) -> List a
concatMaybes maybeXs =
    case maybeXs of
        (Just x) :: xs ->
            x :: concatMaybes xs

        _ :: xs ->
            concatMaybes xs

        _ ->
            []


onCheckStopPropagation : msg -> Attribute msg
onCheckStopPropagation msg =
    let
        alwaysStopPropagation m =
            ( m, True )
    in
    stopPropagationOn "change" (Decode.map alwaysStopPropagation (Decode.succeed msg))


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    let
        alwaysStopPropagation m =
            ( m, True )
    in
    stopPropagationOn "click" (Decode.map alwaysStopPropagation (Decode.succeed msg))


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    let
        alwaysPreventDefault m =
            ( m, True )
    in
    preventDefaultOn "click" (Decode.map alwaysPreventDefault (Decode.succeed msg))
