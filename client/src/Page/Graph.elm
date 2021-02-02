module Page.Graph exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Button
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
import Trackables exposing (Trackables)


type alias Model =
    { today : Date
    , graph : Graph.Model
    }


init : Date -> Trackables -> ( Model, Cmd Msg )
init today trackables =
    ( { today = today
      , graph = initGraph today trackables
      }
    , Dom.getViewportOf "chart"
        |> Task.andThen (\info -> Dom.setViewportOf "chart" info.scene.width 0)
        |> Task.attempt (always NoOp)
    )


initGraph : Date -> Trackables -> Graph.Model
initGraph today trackables =
    Graph.init today
        (Trackables.toDict trackables
            |> Dict.filter
                (\_ t ->
                    case t.data of
                        TText _ ->
                            False

                        _ ->
                            True
                )
            |> Dict.map
                (\_ t ->
                    { name = t.question
                    , colour = t.colour
                    , multiplier = t.multiplier
                    , dataPoints = List.map (Tuple.mapFirst Date.fromRataDie) <| Dict.toList <| Trackable.onlyFloatData t
                    }
                )
        )



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
    | TrackablesChanged Trackables


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FillLinesChecked fl ->
            ( { model | graph = Graph.setFillLines fl model.graph }, Cmd.none )

        ShowPointsChecked sp ->
            ( { model | graph = Graph.setShowPoints sp model.graph }, Cmd.none )

        DataSetHovered id ->
            ( { model | graph = Graph.hoverDataSet id model.graph }, Cmd.none )

        DataSetClicked id ->
            ( { model | graph = Graph.toggleDataSetSelected id model.graph }, Cmd.none )

        DataSetVisibleClicked id ->
            ( { model | graph = Graph.toggleDataSet id model.graph }, Cmd.none )

        DataSetBringForwardClicked id ->
            ( { model | graph = Graph.bringDataSetForward id model.graph }, Cmd.none )

        DataSetPushBackClicked id ->
            ( { model | graph = Graph.pushDataSetBack id model.graph }, Cmd.none )

        GraphMsg graphMsg ->
            ( { model | graph = Graph.update graphMsg model.graph }, Cmd.none )

        TrackablesChanged trackables ->
            ( { model | graph = initGraph model.today trackables }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        dataSets =
            Dict.map
                (\id ds ->
                    let
                        canBringForward =
                            ds.visible && List.head model.graph.dataOrder /= Just id

                        canPushBack =
                            ds.visible && (List.head << List.reverse) model.graph.dataOrder /= Just id

                        canSelect =
                            ds.visible
                    in
                    [ div
                        [ class "p-2 flex first:mt-0 items-center"
                        , classList
                            [ ( "bg-gray-300", model.graph.selectedDataSet == Just id || model.graph.hoveredDataSet == Just id )
                            ]
                        , onMouseEnter <|
                            case model.graph.selectedDataSet of
                                Just _ ->
                                    NoOp

                                _ ->
                                    DataSetHovered (Just id)
                        , onMouseLeave <|
                            case model.graph.selectedDataSet of
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

                        -- , button
                        --     [ class "ml-auto p-2 text-black"
                        --     , classList
                        --         [ ( "text-opacity-30 cursor-default", not canSelect )
                        --         , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canSelect )
                        --         ]
                        --     , onClick (DataSetSelectClicked id)
                        --     , disabled (not canSelect)
                        --     ]
                        --     [ icon "w-6 h-6" <| SolidCrosshairs ]
                        , button
                            [ class "ml-auto text-black"
                            , classList
                                [ ( "text-opacity-30 cursor-default", not canBringForward )
                                , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canBringForward )
                                ]
                            , onClickStopPropagation (DataSetBringForwardClicked id)
                            , disabled (not canBringForward)
                            ]
                            [ icon "w-6 h-6" <| SolidArrowUp
                            ]
                        , button
                            [ class "ml-2 text-black"
                            , classList
                                [ ( "text-opacity-30 cursor-default", not canPushBack )
                                , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canPushBack )
                                ]
                            , onClickStopPropagation (DataSetPushBackClicked id)
                            , disabled (not canPushBack)
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
                model.graph.data
    in
    div [ class "shadow-inner-t-md" ]
        [ h2 [ class "py-4 pb-0 font-bold text-2xl text-center" ]
            [ text "Charts" ]
        , div [ class "mx-4 my-0 flex scrollable-parent", style "height" "300px" ]
            [ viewJustYAxis "flex-grow-0 flex-shrink-0" model.graph
            , viewScrollableContainer [ Html.map GraphMsg <| viewLineGraph "h-full" model.graph ]
            ]
        , div [ class "m-4 mt-4 flex flex-wrap justify-end" ]
            [ label [ class "text-right whitespace-nowrap", for "fill-lines" ] [ text "Colour under curves" ]
            , input
                [ type_ "checkbox"
                , id "fill-lines"
                , class "ml-2"
                , onCheck FillLinesChecked
                , checked model.graph.fillLines
                ]
                []
            , label [ class "ml-8 text-right whitespace-nowrap", for "show-points" ] [ text "Show data points" ]
            , input
                [ type_ "checkbox"
                , id "show-points"
                , class "ml-2"
                , onCheck ShowPointsChecked
                , checked model.graph.showPoints
                ]
                []
            ]
        , div [ class "m-4 mt-4" ] <|
            (List.concatMap (\id -> Maybe.withDefault [] <| Dict.get id dataSets) <| model.graph.dataOrder)
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
