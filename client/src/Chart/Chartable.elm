module Chart.Chartable exposing (Model, Msg(..), buildTrackableOptions, init, toTrackableModel, update, view)

import Browser.Dom as Dom
import Chart.LineChart as Chart exposing (DataSetId(..))
import Colour exposing (Colour)
import Controls
import Date exposing (Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onMouseEnter, onMouseLeave)
import Htmlx
import Listx
import Maybe exposing (Maybe)
import Stringx
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Chartable exposing (Chartable)
import UserData.ChartableId as ChartableId exposing (ChartableId)
import UserData.Trackable as Trackable exposing (TrackableData(..))
import UserData.TrackableId as TrackableId exposing (TrackableId)


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
init userData canDelete chartableId ( chartable, visible ) =
    { chartableId = chartableId
    , name = chartable.name
    , colour = UserData.getChartableColour userData chartableId
    , inverted = chartable.inverted
    , canDelete = canDelete
    , trackables = chartable.sum |> List.filterMap (toTrackableModel userData)
    , visible = visible
    , nameIsPristine = True
    , trackableOptions = buildTrackableOptions userData chartableId
    }


buildTrackableOptions : UserData -> ChartableId -> List ( TrackableId, ( String, Bool ) )
buildTrackableOptions userData chartableId =
    let
        trackablesInUse =
            UserData.getChartable chartableId userData
                |> Maybe.map .sum
                |> Maybe.withDefault []
                |> List.map Tuple.first
    in
    UserData.activeTrackables userData
        |> List.filter
            (\( _, ( t, _ ) ) ->
                case t.data of
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
                        , ( t.question
                          , not (List.member tId trackablesInUse)
                          )
                        )

                else
                    Nothing
            )
        |> (List.sortBy <| String.toUpper << Tuple.first << Tuple.second)


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
    case msg of
        ChartableEditClicked ->
            ( model
            , Dom.focus ("chartable" ++ ChartableId.toString model.chartableId ++ "-name")
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
            ( { model | colour = UserData.getChartableColour userData model.chartableId }
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
            in
            case trackableOptions |> Listx.lookup newTrackableId |> Maybe.map Tuple.first of
                Just question ->
                    ( { model
                        | trackables = model.trackables |> Listx.updateLookupWithKey trackableId (\( _, t ) -> ( newTrackableId, { t | question = question } ))
                        , colour = UserData.getChartableColour userData model.chartableId
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
                        |> Listx.updateLookup trackableId (\t -> { t | multiplier = stringValue, isValid = Trackable.parseMultiplier stringValue /= Nothing })
              }
            , Cmd.none
            )

        TrackableAddClicked (Just trackableId) ->
            case toTrackableModel userData ( trackableId, 1.0 ) of
                Just ( _, trackableModel ) ->
                    ( { model
                        | trackables = model.trackables |> Listx.insertLookup trackableId trackableModel
                        , colour = UserData.getChartableColour userData model.chartableId
                        , trackableOptions = buildTrackableOptions userData model.chartableId
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TrackableDeleteClicked trackableId ->
            ( { model
                | trackables = model.trackables |> (List.filter <| \( tId, _ ) -> tId /= trackableId)
                , colour = UserData.getChartableColour userData model.chartableId
                , trackableOptions = buildTrackableOptions userData model.chartableId
              }
            , Cmd.none
            )

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
        , Colour.class "bg" colour
        , Colour.classUp "border" colour
        , onMouseEnter <| ChartableHovered True
        , onMouseLeave <| ChartableHovered False
        ]
        [ if not isSelected then
            div
                [ class "p-4 flex items-center"
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , Htmlx.onClickStopPropagation <| ChartableVisibleClicked
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , if model.visible then
                    span [ class "ml-4 w-full", Htmlx.onClickStopPropagation NoOp ]
                        [ a [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black pr-8", href "#", target "_self", Htmlx.onClickPreventDefault ChartableEditClicked ]
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
                    , Htmlx.onClickStopPropagation <| ChartableDeleteClicked
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
                    , Htmlx.onClickStopPropagation <| ChartableUpClicked
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
                    , Htmlx.onClickStopPropagation <| ChartableDownClicked
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
                    , Htmlx.onClickStopPropagation <| ChartableVisibleClicked
                    ]
                    [ icon "w-5 h-5" <|
                        if model.visible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , Controls.textbox [ class "ml-4 w-72" ]
                    [ id <| "chartable" ++ ChartableId.toString model.chartableId ++ "-name"
                    , placeholder "Name"
                    ]
                    model.name
                    { isValid = True, isRequired = True, isPristine = model.nameIsPristine }
                    ChartableNameUpdated
                , label [ class "ml-12 flex-shrink-0 flex-grow-0 font-bold text-right whitespace-nowrap", for "inverted" ] [ text "Invert data" ]
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
                    [ class "ml-auto rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                    , Htmlx.onClickStopPropagation ChartableCloseClicked
                    ]
                    [ icon "w-5 h-5" <| SolidTimes ]
                ]
        ]
    , if isSelected then
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
                                (TrackableChanged trackableId)
                                TrackableId.toString
                                TrackableId.fromString
                                (model.trackableOptions |> List.map (\( tId, ( q, visible ) ) -> ( ( tId, visible || tId == trackableId ), q )))
                                Nothing
                                (Just trackableId)
                                { showFilled = False }
                            , icon "mt-3 ml-4 w-4 h-4 flex-grow-0 flex-shrink-0" SolidTimes
                            , Controls.textbox [ class "ml-4 w-20 flex-grow-0 flex-shrink-0" ] [] t.multiplier { isValid = t.isValid, isRequired = True, isPristine = False } (TrackableMultiplierUpdated trackableId)
                            , button
                                [ class "ml-4 rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                                , Htmlx.onClickStopPropagation (TrackableDeleteClicked trackableId)
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
