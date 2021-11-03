module Chart.Trackable exposing (Model, Msg(..), init, update, view)

import Chart.LineChart as Chart
import Colour exposing (Colour)
import Controls
import Date exposing (Unit(..))
import Extra.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onMouseEnter, onMouseLeave)
import Maybe exposing (Maybe)
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId as TId exposing (TrackableId)


type alias Model =
    { trackableId : TrackableId
    , question : String
    , colour : Colour
    , visible : Bool
    , inverted : Bool
    , multiplier : String
    , isValid : Bool
    , canDelete : Bool
    , options : List ( TrackableId, ( String, Bool ) )
    }


init : List ( TrackableId, ( String, Bool ) ) -> Bool -> TrackableId -> Trackable -> String -> Bool -> Bool -> Model
init options canDelete trackableId trackable multiplier inverted visible =
    { trackableId = trackableId
    , question = T.question trackable
    , colour = T.colour trackable
    , visible = visible
    , inverted = inverted
    , canDelete = canDelete
    , multiplier = multiplier
    , isValid = True
    , options = options
    }


type Msg
    = NoOp
    | TrackableHovered Bool
    | TrackableEditClicked
    | TrackableChanged (Maybe TrackableId)
    | TrackableCloseClicked
    | TrackableVisibleClicked
    | TrackableUpClicked
    | TrackableDownClicked
    | TrackableInvertedChanged Bool
    | TrackableDeleteClicked
    | TrackableMultiplierUpdated String
    | TrackableAddClicked


update : UserData -> Maybe (List ( TrackableId, ( String, Bool ) )) -> Msg -> Model -> ( Model, Cmd Msg )
update userData optionsM msg model =
    let
        options =
            optionsM |> Maybe.withDefault model.options
    in
    case msg of
        TrackableVisibleClicked ->
            ( { model | visible = not model.visible }
            , Cmd.none
            )

        TrackableInvertedChanged inverted ->
            ( { model | inverted = inverted }
            , Cmd.none
            )

        TrackableMultiplierUpdated stringValue ->
            ( { model | multiplier = stringValue, isValid = T.parseMultiplier stringValue /= Nothing }
            , Cmd.none
            )

        TrackableChanged (Just newTrackableId) ->
            case userData |> UserData.getTrackable newTrackableId of
                Just trackable ->
                    ( init options True newTrackableId trackable model.multiplier model.inverted model.visible
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : { canMoveUp : Bool, canMoveDown : Bool, isSelected : Bool } -> Model -> List (Html Msg)
view { canMoveUp, canMoveDown, isSelected } model =
    let
        colour =
            if not model.visible then
                Colour.Gray

            else
                model.colour
    in
    [ div
        [ class "border-t-4"
        , Colour.bgClass colour
        , Colour.borderClassDarker colour
        , onMouseEnter <| TrackableHovered True
        , onMouseLeave <| TrackableHovered False
        ]
        [ if not isSelected then
            div
                [ class "p-4 flex items-center"
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , onClickStopPropagation TrackableVisibleClicked
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , if model.visible then
                    span [ class "ml-4 w-full", onClickStopPropagation NoOp ]
                        [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black pr-8", href "#", target "_self", onClickPreventDefault TrackableEditClicked ]
                            [ if isSelected {- selectedDataSet == Just (TrackableId trackableId) -} then
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
                    , onClickStopPropagation TrackableDeleteClicked
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
                    , onClickStopPropagation TrackableUpClicked
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
                    , onClickStopPropagation TrackableDownClicked
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
                    , onClickStopPropagation TrackableVisibleClicked
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , span [ class "ml-4 mr-4 w-full font-bold" ]
                    [ text <| Stringx.withDefault "[no question]" model.question
                    ]
                , label [ class "ml-auto flex-shrink-0 flex-grow-0 font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                , input
                    [ type_ "checkbox"
                    , id "inverted"
                    , class "ml-2 flex-shrink-0 flex-grow-0"
                    , onCheck TrackableInvertedChanged
                    , checked model.inverted
                    ]
                    []
                , span [ class "ml-4" ] []
                , button
                    [ class "ml-auto rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                    , onClickStopPropagation TrackableCloseClicked
                    ]
                    [ icon "w-5 h-5" <| SolidTimes ]
                ]
        ]
    , if isSelected then
        div
            [ class "p-4"
            , Colour.bgClassLighter colour
            ]
            [ div [ class "mt-4 first:mt-0 flex" ]
                [ icon "mt-3 w-4 h-4 ml-0.5 mr-0.5 flex-grow-0 flex-shrink-0" <|
                    SolidEquals
                , Controls.textDropdown "ml-4 w-full h-10"
                    TrackableChanged
                    TId.toString
                    TId.fromString
                    (model.options |> List.map (\( tId, ( q, visible ) ) -> ( ( tId, visible || tId == model.trackableId ), q )))
                    Nothing
                    (Just model.trackableId)
                    { showFilled = False }
                , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] model.multiplier { isValid = model.isValid, isRequired = True, isPristine = False } TrackableMultiplierUpdated
                ]
            , div [ class "mt-4 first:mt-0 flex" ]
                [ Controls.button "ml-9 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey TrackableAddClicked SolidPlusCircle "Add trackable" True ]
            ]

      else
        div [] []
    ]
