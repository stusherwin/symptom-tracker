module Page.Graph exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Date exposing (Date, Unit(..))
import Dict
import Graph exposing (Msg, viewJustYAxis, viewLineGraph)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)
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
    let
        dataSet i ( id, t ) =
            ( id
            , { name = t.question
              , colour = t.colour
              , multiplier = t.multiplier
              , dataPoints = List.map (Tuple.mapFirst Date.fromRataDie) <| Dict.toList <| Trackable.onlyFloatData t
              , order = i
              }
            )
    in
    { today = today
    , data =
        List.indexedMap dataSet <|
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
    }



-- UPDATE


type Msg
    = NoOp
    | GraphMsg Graph.Msg
    | TrackablesChanged Trackables


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
        , div [ class "mx-4 my-0 flex", style "height" "300px" ]
            [ viewJustYAxis "flex-grow-0 flex-shrink-0" model.graph
            , viewScrollableContainer [ Html.map GraphMsg <| viewLineGraph "h-full" model.graph ]
            ]
        , div [ class "m-4 mt-8" ] <|
            (model.graph.data
                |> List.map
                    (\( i, { name, colour } ) ->
                        div []
                            [ text name
                            ]
                    )
            )
        ]


viewScrollableContainer : List (Html msg) -> Html msg
viewScrollableContainer children =
    div [ class "relative flex-grow" ]
        [ div
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
