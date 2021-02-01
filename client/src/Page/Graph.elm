module Page.Graph exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Button
import Date exposing (Date, Unit(..))
import Dict
import Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, onMouseOver, preventDefaultOn, stopPropagationOn)
import Icon exposing (IconType(..))
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
    { today = today
    , data =
        List.indexedMap
            (\i ( id, t ) ->
                ( id
                , { name = t.question
                  , colour = t.colour
                  , multiplier = t.multiplier
                  , dataPoints = List.map (Tuple.mapFirst Date.fromRataDie) <| Dict.toList <| Trackable.onlyFloatData t
                  , order = i
                  , visible = True
                  , selected = False
                  }
                )
            )
        <|
            List.filter
                (\( i, t ) ->
                    case t.data of
                        TText _ ->
                            False

                        _ ->
                            True
                )
            <|
                Trackables.toList
                    trackables
    , selectedPoint = Nothing
    , fillLines = True
    , showPoints = False
    }



-- UPDATE


type Msg
    = NoOp
    | FillLinesChecked Bool
    | ShowPointsChecked Bool
    | DataSetSelectClicked Int
    | DataSetVisibleClicked Int
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

        DataSetSelectClicked i ->
            ( { model | graph = Graph.toggleDataSetSelected i model.graph }, Cmd.none )

        DataSetVisibleClicked i ->
            ( { model | graph = Graph.toggleDataSet i model.graph }, Cmd.none )

        GraphMsg graphMsg ->
            ( { model | graph = Graph.update graphMsg model.graph }, Cmd.none )

        TrackablesChanged trackables ->
            ( { model | graph = initGraph model.today trackables }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
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
            (model.graph.data
                |> List.indexedMap
                    (\i ( _, ds ) ->
                        div
                            [ class "p-2 flex first:mt-0 items-center"
                            , classList
                                [ ( "bg-gray-300", ds.selected )
                                ]
                            ]
                            [ input
                                [ type_ "checkbox"
                                , class "mr-4"
                                , onCheckStopPropagation (DataSetVisibleClicked i)
                                , checked ds.visible
                                ]
                                []
                            , div
                                [ class "w-16 h-8 mr-4 flex-grow-0 flex-shrink-0"
                                ]
                                [ Graph.viewKey "w-full h-full" ds
                                ]
                            , span [ class "mr-4" ] [ text ds.name ]
                            , a
                                ([ href "#"
                                 , target "_self"
                                 , class "ml-auto underline hover:no-underline"
                                 , classList
                                    [ ( "text-gray-300 no-underline cursor-default focus:outline-none", not ds.visible )
                                    ]
                                 , onClickPreventDefault
                                    (if ds.visible then
                                        DataSetSelectClicked i

                                     else
                                        NoOp
                                    )
                                 ]
                                    ++ (if not ds.visible then
                                            [ tabindex -1 ]

                                        else
                                            []
                                       )
                                )
                                [ text <|
                                    if ds.selected then
                                        "deselect"

                                    else
                                        "select"
                                ]
                            ]
                    )
            )
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
