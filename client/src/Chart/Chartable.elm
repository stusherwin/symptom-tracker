module Chart.Chartable exposing (Model, Msg(..), buildTrackableOptions, init, toTrackableModel, update, view)

import Browser.Dom as Dom
import Chart.LineChart as Chart
import Colour exposing (Colour)
import Controls
import Date exposing (Unit(..))
import Extra.Html exposing (..)
import Extra.List as List
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onMouseEnter, onMouseLeave)
import Maybe exposing (Maybe)
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable as C exposing (Chartable)
import UserData.ChartableId as CId exposing (ChartableId)
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId as TId exposing (TrackableId)


type alias Model =
    { chartableId : ChartableId
    , name : String
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


init : UserData -> Bool -> ChartableId -> ( Chartable, Bool ) -> Model
init userData canDelete chartableId ( c, visible ) =
    { chartableId = chartableId
    , name = C.name c
    , colour = C.colour c
    , inverted = C.isInverted c
    , canDelete = canDelete
    , trackables = C.sum c |> List.map toTrackableModel
    , visible = visible
    , nameIsPristine = True
    , trackableOptions = buildTrackableOptions userData chartableId
    }


buildTrackableOptions : UserData -> ChartableId -> List ( TrackableId, ( String, Bool ) )
buildTrackableOptions userData chartableId =
    let
        trackablesInUse =
            UserData.getChartable chartableId userData
                |> Maybe.map C.sum
                |> Maybe.withDefault []
                |> List.map Tuple.first
    in
    UserData.activeTrackables userData
        |> List.filter
            (\( _, ( t, _ ) ) ->
                case T.responses t of
                    TText _ ->
                        False

                    _ ->
                        True
            )
        |> List.filterMap
            (\( tId, ( t, visible ) ) ->
                if visible then
                    Just
                        ( tId
                        , ( T.question t
                          , not (List.member tId trackablesInUse)
                          )
                        )

                else
                    Nothing
            )
        |> (List.sortBy <| String.toUpper << Tuple.first << Tuple.second)


toTrackableModel : ( TrackableId, ( Trackable, Float ) ) -> ( TrackableId, TrackableModel )
toTrackableModel ( trackableId, ( trackable, multiplier ) ) =
    ( trackableId
    , { question = T.question trackable
      , multiplier = String.fromFloat multiplier
      , isValid = True
      }
    )


type Msg
    = NoOp
    | ChartableHovered Bool
    | ChartableEditClicked
    | ChartableCloseClicked
    | ChartableVisibleClicked
    | ChartableUpClicked
    | ChartableDownClicked
    | ChartableChanged (Maybe ChartableId)
    | ChartableNameUpdated String
    | ChartableColourUpdated (Maybe Colour)
    | ChartableInvertedChanged Bool
    | ChartableDeleteClicked
    | TrackableChanged TrackableId (Maybe TrackableId)
    | TrackableMultiplierUpdated TrackableId String
    | TrackableAddClicked (Maybe TrackableId)
    | TrackableDeleteClicked TrackableId


update : UserData -> Msg -> Model -> ( Model, Cmd Msg )
update userData msg model =
    case userData |> UserData.getChartable model.chartableId of
        Just c ->
            case msg of
                ChartableEditClicked ->
                    ( model
                    , Dom.focus ("chartable" ++ CId.toString model.chartableId ++ "-name")
                        |> Task.attempt (always NoOp)
                    )

                ChartableVisibleClicked ->
                    ( { model | visible = not model.visible }
                    , Cmd.none
                    )

                ChartableNameUpdated name ->
                    ( { model | name = name, nameIsPristine = False }
                    , Cmd.none
                    )

                ChartableColourUpdated _ ->
                    ( { model | colour = C.colour c }
                    , Cmd.none
                    )

                ChartableInvertedChanged inverted ->
                    ( { model | inverted = inverted }
                    , Cmd.none
                    )

                TrackableChanged trackableId (Just newTrackableId) ->
                    let
                        trackableOptions =
                            buildTrackableOptions userData model.chartableId

                        trackableOptionM =
                            trackableOptions
                                |> List.lookup newTrackableId
                                |> Maybe.map Tuple.first
                    in
                    case trackableOptionM of
                        Just question ->
                            ( { model
                                | trackables = model.trackables |> List.updateLookupWithKey trackableId (\( _, t ) -> ( newTrackableId, { t | question = question } ))
                                , colour = C.colour c
                                , trackableOptions = trackableOptions
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                TrackableMultiplierUpdated trackableId stringValue ->
                    ( { model
                        | trackables =
                            model.trackables
                                |> List.updateLookup trackableId (\t -> { t | multiplier = stringValue, isValid = T.parseMultiplier stringValue /= Nothing })
                      }
                    , Cmd.none
                    )

                TrackableAddClicked (Just trackableId) ->
                    case userData |> UserData.getTrackable trackableId of
                        Just trackable ->
                            ( { model
                                | trackables = model.trackables ++ [ toTrackableModel ( trackableId, ( trackable, 1.0 ) ) ]
                                , colour = C.colour c
                                , trackableOptions = buildTrackableOptions userData model.chartableId
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                TrackableDeleteClicked trackableId ->
                    ( { model
                        | trackables = model.trackables |> (List.filter <| \( tId, _ ) -> tId /= trackableId)
                        , colour = C.colour c
                        , trackableOptions = buildTrackableOptions userData model.chartableId
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : { canMoveUp : Bool, canMoveDown : Bool, isSelected : Bool } -> Model -> List (Html Msg)
view { canMoveUp, canMoveDown, isSelected } model =
    let
        canEditColour =
            List.length model.trackables > 1

        colour =
            if not model.visible then
                Colour.Gray

            else
                model.colour

        newTrackableId =
            model.trackableOptions
                |> List.filter (Tuple.second >> Tuple.second)
                |> List.map Tuple.first
                |> List.head
    in
    [ div
        [ class "border-t-4"
        , Colour.bgClass colour
        , Colour.borderClassDarker colour
        , onMouseEnter <| ChartableHovered True
        , onMouseLeave <| ChartableHovered False
        ]
        [ if not isSelected then
            div
                [ class "p-4 flex items-center"
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , onClickStopPropagation <| ChartableVisibleClicked
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , if model.visible then
                    span [ class "ml-4 w-full", onClickStopPropagation NoOp ]
                        [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black pr-8", href "#", target "_self", onClickPreventDefault ChartableEditClicked ]
                            [ if isSelected {- selectedDataSet == Just (ChartableId chartableId) -} then
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
                        [ text <| Stringx.withDefault "[no name]" model.name
                        ]
                , button
                    [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", not model.canDelete )
                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", model.canDelete )
                        ]
                    , onClickStopPropagation <| ChartableDeleteClicked
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
                    , onClickStopPropagation <| ChartableUpClicked
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
                    , onClickStopPropagation <| ChartableDownClicked
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
                    , onClickStopPropagation <| ChartableVisibleClicked
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , Controls.textbox [ class "ml-4 mr-4 w-72" ]
                    [ id <| "chartable" ++ CId.toString model.chartableId ++ "-name"
                    , placeholder "Name"
                    ]
                    model.name
                    { isValid = True, isRequired = True, isPristine = model.nameIsPristine }
                    ChartableNameUpdated
                , label [ class "ml-auto flex-shrink-0 flex-grow-0 font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
                , input
                    [ type_ "checkbox"
                    , id "inverted"
                    , class "ml-2 flex-shrink-0 flex-grow-0"
                    , onCheck ChartableInvertedChanged
                    , checked model.inverted
                    ]
                    []
                , if canEditColour then
                    Controls.colourDropdown "ml-4 flex-shrink-0 flex-grow-0" ChartableColourUpdated (Just model.colour) { showFilled = False }

                  else
                    span [ class "ml-4" ] []
                , button
                    [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                    , onClickStopPropagation ChartableCloseClicked
                    ]
                    [ icon "w-5 h-5" <| SolidTimes ]
                ]
        ]
    , if isSelected then
        div
            [ class "p-4"
            , Colour.bgClassLighter colour
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
                                (TrackableChanged trackableId)
                                TId.toString
                                TId.fromString
                                (model.trackableOptions |> List.map (\( tId, ( q, visible ) ) -> ( ( tId, visible || tId == trackableId ), q )))
                                Nothing
                                (Just trackableId)
                                { showFilled = False }
                            , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                            , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True, isPristine = False } (TrackableMultiplierUpdated trackableId)
                            , button
                                [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                , onClickStopPropagation (TrackableDeleteClicked trackableId)
                                ]
                                [ icon "w-5 h-5" <| SolidTrashAlt ]
                            ]
                    )
            )
                ++ [ div [ class "mt-4 first:mt-0 flex" ]
                        [ Controls.button "ml-9 flex-grow-0 flex-shrink-0 whitespace-nowrap" Controls.ButtonGrey (TrackableAddClicked newTrackableId) SolidPlusCircle "Add trackable" (model.trackableOptions |> List.any (Tuple.second << Tuple.second)) ]
                   ]

      else
        div [] []
    ]
