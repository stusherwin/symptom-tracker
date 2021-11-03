module Chart.Chartable exposing (Model, Msg(..), init, toTrackableModel, view)

import Chart.LineChart as Chart
import Colour exposing (Colour)
import Controls
import Date exposing (Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onMouseEnter, onMouseLeave)
import Htmlx
import Maybe exposing (Maybe)
import Svg.Icon exposing (IconType(..), icon)
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as Chartable exposing (Chartable)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.Trackable exposing (TrackableData(..))
import UserData.TrackableId as TrackableId exposing (TrackableId)


type alias Model =
    { name : String
    , colour : Colour
    , visible : Bool
    , inverted : Bool
    , canDelete : Bool
    , nameIsPristine : Bool
    , trackables : List ( TrackableId, TrackableModel )
    , trackableOptions : List ( TrackableId, ( String, Bool ) )
    }


type alias TrackableModel =
    { question : String
    , multiplier : String
    , isValid : Bool
    }


init : UserData -> List ( TrackableId, ( String, Bool ) ) -> Bool -> ( Chartable, Bool ) -> Model
init userData trackableOptions canDelete ( chartable, visible ) =
    { name = chartable.name
    , colour = UserData.getChartableColour userData chartable
    , inverted = chartable.inverted
    , canDelete = canDelete
    , trackables = chartable.sum |> List.filterMap (toTrackableModel userData)
    , visible = visible
    , nameIsPristine = True
    , trackableOptions = trackableOptions
    }


toTrackableModel : UserData -> ( TrackableId, Float ) -> Maybe ( TrackableId, TrackableModel )
toTrackableModel userData ( trackableId, multiplier ) =
    userData
        |> UserData.getTrackable trackableId
        |> Maybe.map
            (\{ question } ->
                ( trackableId
                , { question = question
                  , multiplier = String.fromFloat multiplier
                  , isValid = True
                  }
                )
            )


type Msg
    = NoOp
    | ChartableHovered (Maybe ChartableId)
    | ChartableEditClicked ChartableId
    | ChartableCloseClicked
    | ChartableVisibleClicked ChartableId
    | ChartableUpClicked ChartableId
    | ChartableDownClicked ChartableId
    | ChartableChanged ChartableId (Maybe ChartableId)
    | ChartableNameUpdated ChartableId String
    | ChartableColourUpdated ChartableId (Maybe Colour)
    | ChartableInvertedChanged ChartableId Bool
    | ChartableAddClicked
    | ChartableToAddChanged (Maybe ChartableId)
    | ChartableAddConfirmClicked
    | ChartableAddCancelClicked
    | ChartableDeleteClicked ChartableId
    | TrackableChanged ChartableId TrackableId (Maybe TrackableId)
    | TrackableMultiplierUpdated ChartableId TrackableId String
    | TrackableAddClicked ChartableId
    | TrackableDeleteClicked ChartableId TrackableId
    | UserDataUpdated UserData


view : Maybe ChartableId -> Maybe ChartableId -> Maybe ChartableId -> ( ChartableId, Model ) -> List (Html Msg)
view first last selectedChartable ( chartableId, model ) =
    let
        canMoveUp =
            first /= Just chartableId

        canMoveDown =
            last /= Just chartableId

        canEditColour =
            List.length model.trackables > 1

        options =
            model.trackableOptions
                |> List.map
                    (\( tId, ( question, visible ) ) ->
                        ( ( tId
                          , visible
                                && (not <|
                                        List.member tId <|
                                            (model.trackables
                                                |> List.map Tuple.first
                                            )
                                   )
                          )
                        , question
                        )
                    )

        colour =
            if not model.visible then
                Colour.Gray

            else
                model.colour

        -- if not model.visible || not (selectedChartable == Nothing || selectedChartable == Just chartableId) then
        --     Colour.Gray
        -- else
        --     model.colour
    in
    [ div
        [ class "border-t-4"
        , Colour.class "bg" colour
        , Colour.classUp "border" colour
        , onMouseEnter <| ChartableHovered (Just chartableId)
        , onMouseLeave <| ChartableHovered Nothing
        ]
        [ if selectedChartable /= Just chartableId then
            div
                [ class "p-4 flex items-center"
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartableId
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , if model.visible then
                    span [ class "ml-4 w-full", Htmlx.onClickStopPropagation NoOp ]
                        [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black pr-8", href "#", target "_self", Htmlx.onClickPreventDefault (ChartableEditClicked chartableId) ]
                            [ if selectedChartable == Just chartableId then
                                icon "w-5 h-5 relative -ml-1 mr-0.5" SolidCaretRight

                              else
                                span [] []
                            , span []
                                [ text <|
                                    if String.isEmpty model.name then
                                        "[no name]"

                                    else
                                        model.name
                                ]
                            , icon "absolute right-0 w-5 h-5" SolidPencilAlt
                            ]
                        ]

                  else
                    span [ class "ml-4 w-full font-bold" ]
                        [ text <|
                            if String.isEmpty model.name then
                                "[no name]"

                            else
                                model.name
                        ]
                , button
                    [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", not model.canDelete )
                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", model.canDelete )
                        ]
                    , Htmlx.onClickStopPropagation <| ChartableDeleteClicked chartableId
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
                    , Htmlx.onClickStopPropagation <| ChartableUpClicked chartableId
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
                    , Htmlx.onClickStopPropagation <| ChartableDownClicked chartableId
                    , disabled (not canMoveDown)
                    ]
                    [ icon "w-5 h-5" <| SolidArrowDown
                    ]
                ]

          else
            div
                [ class "px-4 flex items-center"
                , classList
                    [ ( "py-1", canEditColour )
                    , ( "py-2", not canEditColour )
                    ]
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation <| ChartableVisibleClicked chartableId
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , Controls.textbox [ class "ml-4 w-72" ]
                    [ id <| "chartable" ++ ChartableId.toString chartableId ++ "-name"
                    , placeholder "Name"
                    ]
                    model.name
                    { isValid = True, isRequired = True, isPristine = model.nameIsPristine }
                    (ChartableNameUpdated chartableId)
                , label [ class "ml-12 flex-shrink-0 flex-grow-0 font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                , input
                    [ type_ "checkbox"
                    , id "inverted"
                    , class "ml-2 flex-shrink-0 flex-grow-0"
                    , onCheck (ChartableInvertedChanged chartableId)
                    , checked model.inverted
                    ]
                    []
                , if canEditColour then
                    Controls.colourDropdown "ml-4 flex-shrink-0 flex-grow-0" (ChartableColourUpdated chartableId) (Just model.colour) { showFilled = False }

                  else
                    span [ class "ml-4" ] []
                , button
                    [ class "ml-auto rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                    , Htmlx.onClickStopPropagation ChartableCloseClicked
                    ]
                    [ icon "w-5 h-5" <| SolidTimes ]
                ]
        ]
    , if selectedChartable == Just chartableId then
        div
            [ class "p-4"
            , Colour.classDown "bg" colour
            ]
        <|
            (model.trackables
                |> List.indexedMap
                    (\i ( trackableId, t ) ->
                        div [ class "mt-4 first:mt-0 flex" ]
                            [ icon "mt-3 w-4 h-4 ml-0.5 mr-0.5 flex-grow-0 flex-shrink-0" <|
                                if i == 0 then
                                    SolidEquals

                                else
                                    SolidPlus
                            , Controls.textDropdown "ml-4 w-full h-10"
                                (TrackableChanged chartableId trackableId)
                                TrackableId.toString
                                TrackableId.fromString
                                (options |> List.map (\( ( tId, visible ), q ) -> ( ( tId, visible || tId == trackableId ), q )))
                                Nothing
                                (Just trackableId)
                                { showFilled = False }
                            , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                            , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True, isPristine = False } (TrackableMultiplierUpdated chartableId trackableId)
                            , button
                                [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                , Htmlx.onClickStopPropagation (TrackableDeleteClicked chartableId trackableId)
                                ]
                                [ icon "w-5 h-5" <| SolidTrashAlt ]
                            ]
                    )
            )
                ++ [ div [ class "mt-4 first:mt-0 flex" ]
                        [ Controls.button "ml-9 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey (TrackableAddClicked chartableId) SolidPlusCircle "Add trackable" (options |> List.any (Tuple.second << Tuple.first)) ]
                   ]

      else
        div [] []
    ]
