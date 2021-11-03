module Chart.Trackable exposing (Model, Msg(..), init, view)

import Chart.LineChart as Chart exposing (DataSetId(..))
import Colour exposing (Colour)
import Controls
import Date exposing (Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onMouseEnter, onMouseLeave)
import Htmlx
import Maybe exposing (Maybe)
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as Chartable exposing (Chartable)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.LineChart as LineChart exposing (TrackableData)
import UserData.Trackable as Trackable exposing (Trackable, TrackableData(..))
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Model =
    { question : String
    , colour : Colour
    , visible : Bool
    , inverted : Bool
    , multiplier : String
    , isValid : Bool
    , canDelete : Bool
    }


init : Bool -> Trackable -> Float -> Bool -> Bool -> Model
init canDelete trackable multiplier inverted visible =
    { question = trackable.question
    , colour = trackable.colour
    , visible = visible
    , inverted = inverted
    , canDelete = canDelete
    , multiplier = String.fromFloat multiplier
    , isValid = True
    }


type Msg
    = NoOp
    | TrackableHovered (Maybe TrackableId)
    | TrackableEditClicked TrackableId
    | TrackableCloseClicked
    | TrackableVisibleClicked TrackableId
    | TrackableUpClicked TrackableId
    | TrackableDownClicked TrackableId
    | TrackableInvertedChanged TrackableId Bool
    | TrackableDeleteClicked TrackableId
    | TrackableMultiplierUpdated TrackableId String


view : Maybe DataSetId -> Maybe DataSetId -> Maybe DataSetId -> ( TrackableId, Model ) -> List (Html Msg)
view first last selectedDataSet ( trackableId, model ) =
    let
        canMoveUp =
            first /= Just (TrackableId trackableId)

        canMoveDown =
            last /= Just (TrackableId trackableId)

        colour =
            if not model.visible then
                Colour.Gray

            else
                model.colour
    in
    [ div
        [ class "border-t-4"
        , Colour.class "bg" colour
        , Colour.classUp "border" colour
        , onMouseEnter <| TrackableHovered (Just trackableId)
        , onMouseLeave <| TrackableHovered Nothing
        ]
        [ if selectedDataSet /= Just (TrackableId trackableId) then
            div
                [ class "p-4 flex items-center"
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation <| TrackableVisibleClicked trackableId
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , if model.visible then
                    span [ class "ml-4 w-full", Htmlx.onClickStopPropagation NoOp ]
                        [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black pr-8", href "#", target "_self", Htmlx.onClickPreventDefault (TrackableEditClicked trackableId) ]
                            [ if selectedDataSet == Just (TrackableId trackableId) then
                                icon "w-5 h-5 relative -ml-1 mr-0.5" SolidCaretRight

                              else
                                span [] []
                            , span [ class "w-full font-bold" ]
                                [ text <| Stringx.withDefault "[no question]" model.question
                                ]
                            , icon "absolute right-0 w-5 h-5" SolidPencilAlt
                            ]
                        ]

                  else
                    span [ class "ml-4 w-full font-bold" ]
                        [ text <| Stringx.withDefault "[no question]" model.question
                        ]
                , button
                    [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", not model.canDelete )
                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", model.canDelete )
                        ]
                    , Htmlx.onClickStopPropagation <| TrackableDeleteClicked trackableId
                    , disabled (not model.canDelete)
                    ]
                    [ icon "w-5 h-5" <| SolidTrashAlt
                    ]
                , button
                    [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", not canMoveUp )
                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveUp )
                        ]
                    , Htmlx.onClickStopPropagation <| TrackableUpClicked trackableId
                    , disabled (not canMoveUp)
                    ]
                    [ icon "w-5 h-5" <| SolidArrowUp
                    ]
                , button
                    [ class "ml-1 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", not canMoveDown )
                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveDown )
                        ]
                    , Htmlx.onClickStopPropagation <| TrackableDownClicked trackableId
                    , disabled (not canMoveDown)
                    ]
                    [ icon "w-5 h-5" <| SolidArrowDown
                    ]
                ]

          else
            div
                [ class "p-4 flex items-center"
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation <| TrackableVisibleClicked trackableId
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , span [ class "ml-4 w-full font-bold" ]
                    [ text <| Stringx.withDefault "[no question]" model.question
                    ]
                , label [ class "ml-12 flex-shrink-0 flex-grow-0 font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                , input
                    [ type_ "checkbox"
                    , id "inverted"
                    , class "ml-2 flex-shrink-0 flex-grow-0"
                    , onCheck (TrackableInvertedChanged trackableId)
                    , checked model.inverted
                    ]
                    []
                , span [ class "ml-4" ] []
                , button
                    [ class "ml-auto rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                    , Htmlx.onClickStopPropagation TrackableCloseClicked
                    ]
                    [ icon "w-5 h-5" <| SolidTimes ]
                ]
        ]
    , if selectedDataSet == Just (TrackableId trackableId) then
        div
            [ class "p-4"
            , Colour.classDown "bg" colour
            ]
            [ div [ class "mt-4 first:mt-0 flex" ]
                [ icon "mt-3 w-4 h-4 ml-0.5 mr-0.5 flex-grow-0 flex-shrink-0" <|
                    SolidEquals
                , span [ class "ml-4 mt-2 w-full font-bold" ]
                    [ text <| Stringx.withDefault "[no question]" model.question
                    ]
                , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] model.multiplier { isValid = model.isValid, isRequired = True, isPristine = False } (TrackableMultiplierUpdated trackableId)
                ]
            ]

      else
        div [] []
    ]