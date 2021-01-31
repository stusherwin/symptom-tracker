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
        dataSet : ( Int, Trackable ) -> Maybe ( Int, Graph.DataSet )
        dataSet ( tId, { question, colour, multiplier, data } ) =
            case data of
                TYesNo answers ->
                    Just
                        ( tId
                        , { name = question
                          , colour = colour
                          , multiplier = multiplier
                          , dataPoints =
                                List.map
                                    (Tuple.mapBoth Date.fromRataDie
                                        (\v ->
                                            if v then
                                                1

                                            else
                                                0
                                        )
                                    )
                                <|
                                    Dict.toList answers
                          }
                        )

                TIcon _ answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie toFloat) <| Dict.toList answers } )

                TScale _ _ answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie toFloat) <| Dict.toList answers } )

                TInt answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie toFloat) <| Dict.toList answers } )

                TFloat answers ->
                    Just ( tId, { name = question, colour = colour, multiplier = multiplier, dataPoints = List.map (Tuple.mapBoth Date.fromRataDie identity) <| Dict.toList answers } )

                TText _ ->
                    Nothing
    in
    { today = today
    , data = concatMaybes <| List.map dataSet <| Trackables.toList trackables
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
