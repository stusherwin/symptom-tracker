module Page.Trackables exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Chart.LineChart as Chart
import Colour exposing (Colour(..))
import Controls
import Dict
import Extra.Html exposing (..)
import Extra.List as List
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick)
import IdDict
import Platform.Cmd as Cmd
import Svg.Icon exposing (IconType(..), icon)
import Task
import UserData exposing (UserData)
import UserData.Chartable as C exposing (Chartable)
import UserData.ChartableId as CId exposing (ChartableId)
import UserData.LineChart as LC exposing (LineChart(..))
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId as TId exposing (TrackableId)


type alias Model =
    { trackables : List ( TrackableId, TrackableModel )
    , editState : EditState
    }


type EditState
    = NotEditing
    | EditingTrackable TrackableId


type alias TrackableModel =
    { question : String
    , colour : Colour
    , answerType : AnswerType
    , scaleOptions : ScaleOptions
    , iconOptions : IconOptions
    , canDelete : Bool
    , answerTypes : List ( AnswerType, Bool )
    , isVisible : Bool
    }


type alias ScaleOptions =
    { from : Int
    , fromMin : Int
    , fromMax : Int
    , to : Int
    , toMin : Int
    , toMax : Int
    }


type alias IconOptions =
    Array IconOption


type alias IconOption =
    { iconType : IconType
    , canDelete : Bool
    }


type AnswerType
    = AYesNo
    | AIcon
    | AScale
    | AInt
    | AFloat
    | AText


init : UserData -> Model
init userData =
    { trackables = UserData.activeTrackables userData |> List.mapLookup (toModel userData)
    , editState = NotEditing
    }


toModel : UserData -> TrackableId -> ( Trackable, Bool ) -> TrackableModel
toModel userData tId ( t, visible ) =
    let
        floatData =
            Dict.values <| T.maybeFloatData t
    in
    { question = T.question t
    , colour = T.colour t
    , isVisible = visible
    , answerType =
        case T.responses t of
            TYesNo _ ->
                AYesNo

            TIcon _ _ ->
                AIcon

            TScale _ _ _ ->
                AScale

            TInt _ ->
                AInt

            TFloat _ ->
                AFloat

            TText _ ->
                AText
    , scaleOptions =
        let
            ( maybeMin, maybeMax ) =
                ( Maybe.map truncate << List.minimum << List.concatMaybes <| floatData
                , Maybe.map truncate << List.maximum << List.concatMaybes <| floatData
                )

            ( from, to ) =
                case T.responses t of
                    TScale origFrom origTo _ ->
                        ( origFrom, origTo )

                    _ ->
                        case ( maybeMin, maybeMax ) of
                            ( Just min, Just max ) ->
                                ( min, max )

                            _ ->
                                ( 0, 20 )
        in
        { from = from
        , to = to
        , fromMin = 0
        , fromMax =
            case maybeMin of
                Just min ->
                    min

                _ ->
                    19
        , toMin =
            case maybeMax of
                Just max ->
                    max

                _ ->
                    1
        , toMax = 20
        }
    , iconOptions =
        case T.responses t of
            TIcon options values ->
                Array.indexedMap (\i o -> { iconType = o, canDelete = not <| List.any (\v -> v >= i) (Dict.values values) }) options

            _ ->
                let
                    max =
                        Maybe.map truncate << List.maximum << List.concatMaybes <| floatData
                in
                case max of
                    Just m ->
                        Array.repeat (m + 1) { iconType = SolidQuestionCircle, canDelete = False }

                    _ ->
                        Array.fromList
                            [ { iconType = SolidQuestionCircle, canDelete = False }
                            , { iconType = SolidQuestionCircle, canDelete = False }
                            ]
    , canDelete =
        (not <| T.hasData t)
            && (UserData.lineCharts userData
                    |> IdDict.values
                    |> List.concatMap (Array.toList << LC.dataSets)
                    |> List.map Tuple.first
                    |> List.filterMap
                        (\dataSetId ->
                            case dataSetId of
                                LC.TrackableElement { trackableId } ->
                                    Just trackableId

                                _ ->
                                    Nothing
                        )
                    |> List.member tId
                    |> not
               )
    , answerTypes =
        [ ( AYesNo
          , List.all
                (\val ->
                    case val of
                        Just v ->
                            floor v == ceiling v && v >= 0 && v <= 1

                        _ ->
                            False
                )
                floatData
          )
        , ( AIcon
          , List.all
                (\val ->
                    case val of
                        Just v ->
                            floor v == ceiling v && v >= 0 && v <= 10

                        _ ->
                            False
                )
                floatData
          )
        , ( AScale
          , List.all
                (\val ->
                    case val of
                        Just v ->
                            floor v == ceiling v && v >= 0 && v <= 20

                        _ ->
                            False
                )
                floatData
          )
        , ( AInt
          , List.all
                (\val ->
                    case val of
                        Just v ->
                            floor v == ceiling v

                        _ ->
                            False
                )
                floatData
          )
        , ( AFloat
          , List.all
                (\val ->
                    case val of
                        Just _ ->
                            True

                        _ ->
                            False
                )
                floatData
          )
        , ( AText, True )
        ]
    }



-- UPDATE


type Msg
    = NoOp
    | TrackableEditClicked TrackableId
    | TrackableCloseClicked
    | TrackableColourUpdated TrackableId (Maybe Colour)
    | TrackableQuestionUpdated TrackableId String
    | TrackableAnswerTypeUpdated TrackableId (Maybe AnswerType)
    | TrackableDeleteClicked TrackableId
    | TrackableAddClicked
    | TrackableVisibleClicked TrackableId
    | TrackableUpClicked TrackableId
    | TrackableDownClicked TrackableId
    | ScaleFromUpdated TrackableId (Maybe Int)
    | ScaleToUpdated TrackableId (Maybe Int)
    | IconUpdated TrackableId Int (Maybe IconType)
    | IconAddClicked TrackableId
    | IconDeleteClicked TrackableId Int
    | UserDataUpdated UserData


update : UserData -> Msg -> Model -> ( Model, Cmd Msg )
update userData msg model =
    let
        setUserData userData_ ( m, cmd ) =
            ( m
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )

        updateUserData fn ( m, cmd ) =
            let
                userData_ =
                    userData |> fn
            in
            ( m
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )
    in
    case msg of
        TrackableEditClicked id ->
            ( model, Cmd.none )
                |> (Tuple.mapFirst <| \m -> { m | editState = EditingTrackable id })

        TrackableCloseClicked ->
            ( model, Cmd.none )
                |> (Tuple.mapFirst <| \m -> { m | editState = NotEditing })

        TrackableUpClicked id ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.moveTrackableUp id)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.moveHeadwardsBy Tuple.first id })

        TrackableDownClicked id ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.moveTrackableDown id)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.moveTailwardsBy Tuple.first id })

        TrackableVisibleClicked id ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.toggleTrackableVisible id)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.updateLookup id (\t -> { t | isVisible = not t.isVisible }) })

        TrackableColourUpdated id (Just colour) ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.setColour colour)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.updateLookup id (\q -> { q | colour = colour }) })

        TrackableQuestionUpdated id question ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.setQuestion question)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.updateLookup id (\q -> { q | question = question }) })

        TrackableAnswerTypeUpdated id (Just answerType) ->
            ( model, Cmd.none )
                |> (setUserData <|
                        case model.trackables |> List.lookup id of
                            Just q ->
                                userData
                                    |> (UserData.updateTrackable id <|
                                            case answerType of
                                                AYesNo ->
                                                    T.convertToYesNo

                                                AIcon ->
                                                    T.convertToIcon (Array.map .iconType q.iconOptions)

                                                AScale ->
                                                    T.convertToScale q.scaleOptions.from q.scaleOptions.to

                                                AInt ->
                                                    T.convertToInt

                                                AFloat ->
                                                    T.convertToFloat

                                                AText ->
                                                    T.convertToText
                                       )

                            _ ->
                                userData
                   )
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.updateLookup id (\q -> { q | answerType = answerType }) })

        TrackableAddClicked ->
            let
                ( idM, userData_ ) =
                    userData
                        |> UserData.addTrackable
                            { question = ""
                            , colour = Colour.Red
                            , responses = TYesNo Dict.empty
                            }
            in
            case idM of
                Just ( id, trackable ) ->
                    ( model, Cmd.none )
                        |> setUserData userData_
                        |> (Tuple.mapFirst <|
                                \m ->
                                    { m
                                        | trackables = model.trackables |> List.insertLookup id (toModel userData_ id ( trackable, True ))
                                        , editState = EditingTrackable id
                                    }
                           )
                        |> (Tuple.mapSecond <|
                                \c ->
                                    Cmd.batch
                                        [ c
                                        , Dom.getViewport
                                            |> Task.andThen (\info -> Dom.setViewport 0 info.scene.height)
                                            |> (Task.andThen <| always <| Dom.focus ("q-" ++ TId.toString id))
                                            |> Task.attempt (always NoOp)
                                        ]
                           )

                _ ->
                    ( model, Cmd.none )

        TrackableDeleteClicked id ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.deleteTrackable id)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.deleteLookup id })

        ScaleFromUpdated id (Just from) ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.updateScaleFrom from)
                |> (Tuple.mapFirst <|
                        \m ->
                            { m
                                | trackables =
                                    model.trackables
                                        |> List.updateLookup id
                                            (\q ->
                                                let
                                                    scaleOptions =
                                                        q.scaleOptions
                                                in
                                                { q | scaleOptions = { scaleOptions | from = from } }
                                            )
                            }
                   )

        ScaleToUpdated id (Just to) ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.updateScaleTo to)
                |> (Tuple.mapFirst <|
                        \m ->
                            { m
                                | trackables =
                                    model.trackables
                                        |> List.updateLookup id
                                            (\q ->
                                                let
                                                    scaleOptions =
                                                        q.scaleOptions
                                                in
                                                { q | scaleOptions = { scaleOptions | to = to } }
                                            )
                            }
                   )

        IconUpdated id i (Just iconType) ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.updateIcon i iconType)
                |> (Tuple.mapFirst <|
                        \m ->
                            { m
                                | trackables =
                                    model.trackables
                                        |> List.updateLookup id
                                            (\q ->
                                                { q
                                                    | iconOptions =
                                                        case Array.get i q.iconOptions of
                                                            Just o ->
                                                                Array.set i { o | iconType = iconType } q.iconOptions

                                                            _ ->
                                                                q.iconOptions
                                                }
                                            )
                            }
                   )

        IconAddClicked id ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.addIcon SolidQuestionCircle)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.updateLookup id (\q -> { q | iconOptions = Array.push { iconType = SolidQuestionCircle, canDelete = True } q.iconOptions }) })

        IconDeleteClicked id i ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.deleteIcon i)
                |> (Tuple.mapFirst <| \m -> { m | trackables = model.trackables |> List.updateLookup id (\q -> { q | iconOptions = Array.append (Array.slice 0 i q.iconOptions) (Array.slice (i + 1) (Array.length q.iconOptions) q.iconOptions) }) })

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { trackables, editState } =
    let
        firstTrackable =
            (Maybe.map Tuple.first << List.head) trackables

        lastTrackable =
            (Maybe.map Tuple.first << List.head << List.reverse) trackables
    in
    div [ class "bg-white" ]
        [ h2 [ class "py-4 font-bold text-2xl text-center" ]
            [ text <| "Trackables" ]
        , div [ class "" ] <|
            List.map (viewTrackable editState firstTrackable lastTrackable) trackables
                ++ [ div [ class "bg-gray-300 border-t-4 border-gray-400 flex" ]
                        [ Controls.button "mx-4 my-2" Controls.ButtonGrey TrackableAddClicked SolidPlusCircle "Add new trackable" True
                        ]
                   ]
        ]


viewTrackable : EditState -> Maybe TrackableId -> Maybe TrackableId -> ( TrackableId, TrackableModel ) -> Html Msg
viewTrackable editState first last ( id, q ) =
    let
        canMoveUp =
            first /= Just id

        canMoveDown =
            last /= Just id

        answerTypes =
            List.map
                (\( answerType, enabled ) ->
                    ( ( answerType, enabled )
                    , case answerType of
                        AYesNo ->
                            "Yes/no"

                        AIcon ->
                            "Icons"

                        AScale ->
                            "Scale"

                        AInt ->
                            "Whole number"

                        AFloat ->
                            "Decimal"

                        AText ->
                            "Text"
                    )
                )
                q.answerTypes

        viewScaleOptions =
            case q.answerType of
                AScale ->
                    [ div [ class "mt-4 flex" ]
                        [ span [ class "mr-2 py-1 border-4 border-transparent font-bold" ] [ text "From" ]
                        , Controls.textDropdown
                            "mr-2 w-20 h-10"
                            (ScaleFromUpdated id)
                            String.fromInt
                            String.toInt
                            (List.range 0 19
                                |> List.map
                                    (\i ->
                                        ( ( i, i >= q.scaleOptions.fromMin && i <= q.scaleOptions.fromMax ), String.fromInt i )
                                    )
                            )
                            Nothing
                            (Just q.scaleOptions.from)
                            { showFilled = False }
                        , span [ class "mr-2 py-1 border-4 border-transparent font-bold" ] [ text "to" ]
                        , Controls.textDropdown
                            "w-20 h-10"
                            (ScaleToUpdated id)
                            String.fromInt
                            String.toInt
                            (List.range 1 20
                                |> List.map
                                    (\i ->
                                        ( ( i, i >= q.scaleOptions.toMin && i <= q.scaleOptions.toMax ), String.fromInt i )
                                    )
                            )
                            Nothing
                            (Just q.scaleOptions.to)
                            { showFilled = False }
                        ]
                    ]

                _ ->
                    []

        viewIconOptions =
            case q.answerType of
                AIcon ->
                    [ div [ class "mt-2" ]
                        [ ul [ class "flex flex-wrap" ] <|
                            (List.indexedMap
                                (\i { iconType, canDelete } ->
                                    li [ class "mt-4 mr-2 w-32 flex items-start" ]
                                        [ div [ class "flex flex-col items-center" ] <|
                                            [ Controls.iconDropdown "flex-shrink-0 flex-grow-0" (IconUpdated id i) (Just iconType) { showFilled = False }
                                            , div []
                                                [ span [ class "text-lg font-bold text-opacity-70" ] [ text <| String.fromInt i ]
                                                ]
                                            ]
                                        , button
                                            [ class "mt-1 rounded p-2 text-black"
                                            , classList
                                                [ ( "text-opacity-30 cursor-default", not canDelete )
                                                , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", canDelete )
                                                ]
                                            , onClick (IconDeleteClicked id i)
                                            , disabled (not canDelete)
                                            ]
                                            [ icon "w-5 h-5" <| SolidTrashAlt
                                            ]
                                        ]
                                )
                             <|
                                Array.toList q.iconOptions
                            )
                                ++ [ li [ class "mt-4 flex items-start" ]
                                        [ Controls.button "mt-1" Controls.ButtonGrey (IconAddClicked id) SolidPlusCircle "Add" True
                                        ]
                                   ]
                        ]
                    ]

                _ ->
                    []

        colour =
            if not q.isVisible then
                Colour.Gray

            else
                q.colour
    in
    if editState /= EditingTrackable id then
        div []
            [ div
                [ class "p-4 border-t-4 flex"
                , Colour.bgClass colour
                , Colour.borderClassDarker colour
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , onClick <| TrackableVisibleClicked id
                    ]
                    [ icon "w-5 h-5" <|
                        if q.isVisible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , if q.isVisible then
                    span [ class "ml-4 w-full", onClickStopPropagation NoOp ]
                        [ a
                            [ class "block w-full font-bold flex items-center relative text-opacity-70 hover:text-opacity-100 text-black pr-8"
                            , href "#"
                            , target "_self"
                            , onClickPreventDefault (TrackableEditClicked id)
                            ]
                            [ span []
                                [ text <|
                                    if String.isEmpty q.question then
                                        "[no question]"

                                    else
                                        q.question
                                ]
                            , icon "absolute right-0 w-5 h-5" SolidPencilAlt
                            ]
                        ]

                  else
                    span [ class "ml-4 w-full font-bold" ]
                        [ text <|
                            if String.isEmpty q.question then
                                "[no question]"

                            else
                                q.question
                        ]
                , button
                    [ class "ml-4 flex-grow-0 flex-shrink-0 rounded text-black"
                    , classList
                        [ ( "text-opacity-30 cursor-default", not q.canDelete )
                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none", q.canDelete )
                        ]
                    , onClick (TrackableDeleteClicked id)
                    , disabled (not q.canDelete)
                    ]
                    [ icon "w-5 h-5" <| SolidTrashAlt
                    ]
                , button
                    [ class "ml-4 flex-grow-0 flex-shrink-0 text-black focus:outline-none"
                    , classList
                        [ ( "text-opacity-30 cursor-default", not canMoveUp )
                        , ( "text-opacity-70 hover:text-opacity-100 focus:text-opacity-100", canMoveUp )
                        ]
                    , onClick <| TrackableUpClicked id
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
                    , onClick <| TrackableDownClicked id
                    , disabled (not canMoveDown)
                    ]
                    [ icon "w-5 h-5" <| SolidArrowDown
                    ]
                ]
            ]

    else
        div []
            [ div
                [ class "px-4 py-2 border-t-4 flex"
                , Colour.bgClass <|
                    if q.isVisible then
                        q.colour

                    else
                        Colour.Gray
                , Colour.borderClassDarker <|
                    if q.isVisible then
                        q.colour

                    else
                        Colour.Gray
                ]
                [ button
                    [ class "text-black focus:outline-none flex-grow-0 flex-shrink-0 text-opacity-70 hover:text-opacity-100 focus:text-opacity-100"
                    , onClick <| TrackableVisibleClicked id
                    ]
                    [ icon "w-5 h-5" <|
                        if q.isVisible then
                            SolidEye

                        else
                            SolidEyeSlash
                    ]
                , Controls.textbox [ class "w-full ml-4 mr-4" ] [ A.id <| "q-" ++ TId.toString id, placeholder "Question" ] q.question { isValid = True, isRequired = False, isPristine = False } (TrackableQuestionUpdated id)
                , button
                    [ class "ml-auto rounded text-black text-opacity-70 hover:text-opacity-100 focus:text-opacity-100 focus:outline-none"
                    , onClickStopPropagation TrackableCloseClicked
                    ]
                    [ icon "w-5 h-5" <| SolidTimes ]
                ]
            , div
                [ class "py-4 px-4 pt-2 pl-12"
                , Colour.bgClassLighter <|
                    if q.isVisible then
                        q.colour

                    else
                        Colour.Gray
                ]
                [ div [ class "ml-1" ] <|
                    div [ class "flex justify-start items-end" ]
                        [ Controls.textDropdown "w-48 h-10 flex-shrink-0 flex-grow-0" (TrackableAnswerTypeUpdated id) answerTypeToString answerTypeFromString (answerTypes |> List.sortBy (String.toUpper << Tuple.second)) Nothing (Just q.answerType) { showFilled = False }
                        , Controls.colourDropdown "ml-auto relative top-1 flex-shrink-0 flex-grow-0" (TrackableColourUpdated id) (Just q.colour) { showFilled = False }
                        ]
                        :: viewScaleOptions
                        ++ viewIconOptions
                ]
            ]


answerTypeToString : AnswerType -> String
answerTypeToString t =
    case t of
        AYesNo ->
            "AYesNo"

        AIcon ->
            "AIcon"

        AScale ->
            "AScale"

        AInt ->
            "AInt"

        AFloat ->
            "AFloat"

        AText ->
            "AText"


answerTypeFromString : String -> Maybe AnswerType
answerTypeFromString str =
    case str of
        "AYesNo" ->
            Just AYesNo

        "AIcon" ->
            Just AIcon

        "AScale" ->
            Just AScale

        "AInt" ->
            Just AInt

        "AFloat" ->
            Just AFloat

        "AText" ->
            Just AText

        _ ->
            Nothing
