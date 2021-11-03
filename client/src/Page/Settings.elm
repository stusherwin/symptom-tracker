module Page.Settings exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Colour exposing (Colour(..))
import Controls
import Dict
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)
import IdDict exposing (IdDict)
import Listx
import Platform.Cmd as Cmd
import Svg.Icon exposing (IconType(..), icon)
import Task
import UserData exposing (UserData)
import UserData.Trackable as Trackable exposing (Trackable, TrackableData(..), TrackableId)


type alias Model =
    { questions : IdDict TrackableId Question
    , selectedValue : Int
    }


type alias Question =
    { question : String
    , colour : Colour
    , answerType : AnswerType
    , scaleOptions : ScaleOptions
    , iconOptions : IconOptions
    , canDelete : Bool
    , answerTypes : List ( AnswerType, Bool )
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
    { questions = IdDict.map trackableToQuestion <| UserData.trackables userData
    , selectedValue = 4
    }


trackableToQuestion : TrackableId -> Trackable -> Question
trackableToQuestion _ t =
    let
        floatData =
            Dict.values <| Trackable.maybeFloatData t
    in
    { question = t.question
    , colour = t.colour
    , answerType =
        case t.data of
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
                ( Maybe.map truncate << List.minimum << Listx.concatMaybes <| floatData
                , Maybe.map truncate << List.maximum << Listx.concatMaybes <| floatData
                )

            ( from, to ) =
                case t.data of
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
        case t.data of
            TIcon options values ->
                Array.indexedMap (\i o -> { iconType = o, canDelete = not <| List.any (\v -> v >= i) (Dict.values values) }) options

            _ ->
                let
                    max =
                        Maybe.map truncate << List.maximum << Listx.concatMaybes <| floatData
                in
                case max of
                    Just m ->
                        Array.repeat (m + 1) { iconType = SolidQuestionCircle, canDelete = False }

                    _ ->
                        Array.fromList
                            [ { iconType = SolidQuestionCircle, canDelete = False }
                            , { iconType = SolidQuestionCircle, canDelete = False }
                            ]
    , canDelete = not <| Trackable.hasData t
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
    | QuestionColourUpdated TrackableId (Maybe Colour)
    | QuestionTextUpdated TrackableId String
    | QuestionAnswerTypeUpdated TrackableId (Maybe AnswerType)
    | QuestionDeleteClicked TrackableId
    | QuestionAddClicked
    | ScaleFromUpdated TrackableId (Maybe Int)
    | ScaleToUpdated TrackableId (Maybe Int)
    | IconUpdated TrackableId Int (Maybe IconType)
    | IconAddClicked TrackableId
    | IconDeleteClicked TrackableId Int
    | ValueUpdated (Maybe Int)
    | UpdateTrackable (Trackable -> Result String Trackable) TrackableId
    | AddTrackable Trackable
    | DeleteTrackable TrackableId
    | UserDataTrackableAdded ( TrackableId, Trackable )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuestionColourUpdated id (Just colour) ->
            ( { model | questions = IdDict.update id (\q -> { q | colour = colour }) model.questions }
            , Task.perform (UpdateTrackable <| \t -> Ok { t | colour = colour }) <| Task.succeed id
            )

        QuestionTextUpdated id question ->
            ( { model | questions = IdDict.update id (\q -> { q | question = question }) model.questions }
            , Task.perform (UpdateTrackable <| \t -> Ok { t | question = question }) <| Task.succeed id
            )

        QuestionAnswerTypeUpdated id (Just answerType) ->
            ( { model | questions = IdDict.update id (\q -> { q | answerType = answerType }) model.questions }
            , case IdDict.get id model.questions of
                Just q ->
                    Task.perform
                        (UpdateTrackable <|
                            case answerType of
                                AYesNo ->
                                    Trackable.convertToYesNo

                                AIcon ->
                                    Trackable.convertToIcon (Array.map .iconType q.iconOptions)

                                AScale ->
                                    Trackable.convertToScale q.scaleOptions.from q.scaleOptions.to

                                AInt ->
                                    Trackable.convertToInt

                                AFloat ->
                                    Trackable.convertToFloat

                                AText ->
                                    Trackable.convertToText
                        )
                    <|
                        Task.succeed id

                _ ->
                    Cmd.none
            )

        QuestionAddClicked ->
            ( model
            , Task.perform
                (\_ ->
                    AddTrackable
                        { question = ""
                        , colour = Colour.Red
                        , multiplier = 1.0
                        , data = TYesNo Dict.empty
                        }
                )
              <|
                Task.succeed ()
            )

        UserDataTrackableAdded ( id, t ) ->
            ( { model | questions = IdDict.insert id (trackableToQuestion id t) model.questions }
            , Dom.getViewport
                |> Task.andThen (\info -> Dom.setViewport 0 info.scene.height)
                |> (Task.andThen <| always <| Dom.focus ("q-" ++ Trackable.idToString id))
                |> Task.attempt (always NoOp)
            )

        QuestionDeleteClicked id ->
            ( { model | questions = IdDict.delete id model.questions }
            , Task.perform DeleteTrackable <| Task.succeed id
            )

        ScaleFromUpdated id (Just from) ->
            ( { model
                | questions =
                    IdDict.update id
                        (\q ->
                            let
                                scaleOptions =
                                    q.scaleOptions
                            in
                            { q | scaleOptions = { scaleOptions | from = from } }
                        )
                        model.questions
              }
            , Task.perform (UpdateTrackable <| Trackable.updateScaleFrom from) <| Task.succeed id
            )

        ScaleToUpdated id (Just to) ->
            ( { model
                | questions =
                    IdDict.update id
                        (\q ->
                            let
                                scaleOptions =
                                    q.scaleOptions
                            in
                            { q | scaleOptions = { scaleOptions | to = to } }
                        )
                        model.questions
              }
            , Task.perform (UpdateTrackable <| Trackable.updateScaleTo to) <| Task.succeed id
            )

        IconUpdated id i (Just iconType) ->
            ( { model
                | questions =
                    IdDict.update id
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
                        model.questions
              }
            , Task.perform (UpdateTrackable <| Trackable.updateIcon i iconType) <| Task.succeed id
            )

        IconAddClicked id ->
            ( { model | questions = IdDict.update id (\q -> { q | iconOptions = Array.push { iconType = SolidQuestionCircle, canDelete = True } q.iconOptions }) model.questions }
            , Task.perform (UpdateTrackable <| Trackable.addIcon SolidQuestionCircle) <| Task.succeed id
            )

        IconDeleteClicked id i ->
            ( { model | questions = IdDict.update id (\q -> { q | iconOptions = Array.append (Array.slice 0 i q.iconOptions) (Array.slice (i + 1) (Array.length q.iconOptions) q.iconOptions) }) model.questions }
            , Task.perform (UpdateTrackable <| Trackable.deleteIcon i) <| Task.succeed id
            )

        ValueUpdated (Just v) ->
            ( { model | selectedValue = v }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { questions, selectedValue } =
    div [ class "shadow-inner-t-md" ] <|
        [ h2 [ class "py-4 font-bold text-2xl text-center" ]
            [ text <| "Manage your questions" ]

        -- , viewTest selectedValue
        ]
            ++ (IdDict.values <|
                    IdDict.map viewQuestion questions
               )
            ++ [ div [ class "bg-gray-200 border-t-4 border-gray-300 flex" ]
                    [ Controls.button "m-4" Controls.ButtonGrey QuestionAddClicked SolidPlusCircle "Add a question" True
                    ]
               ]


viewTest : Int -> Html Msg
viewTest selectedValue =
    Controls.textDropdown
        "m-4"
        ValueUpdated
        String.fromInt
        String.toInt
        [ ( ( 1, True ), "Carrots" )
        , ( ( 2, False ), "Peas" )
        , ( ( 3, True ), "Cauliflower" )
        , ( ( 4, False ), "Aubergine" )
        , ( ( 5, True ), "Tomatoes" )
        , ( ( 6, True ), "Sundried tomatoes" )
        , ( ( 7, False ), "Chopped tomatoes" )
        , ( ( 8, True ), "Sunflower seeds" )
        , ( ( 9, True ), "Cabbage" )
        , ( ( 10, True ), "Courgettes" )
        ]
        (Just selectedValue)
        { showFilled = False }


viewQuestion : TrackableId -> Question -> Html Msg
viewQuestion id q =
    let
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
                    [ div [ class "mt-6 flex" ]
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
                            (Just q.scaleOptions.to)
                            { showFilled = False }
                        ]
                    ]

                _ ->
                    []

        viewIconOptions =
            case q.answerType of
                AIcon ->
                    [ div [ class "mt-4" ]
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
                                            [ icon "w-6 h-6" <| SolidTrash

                                            -- if canDelete then
                                            --     SolidTrash
                                            -- else
                                            --     SolidTrashBan
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
    in
    div [ class "py-6 px-4 border-t-4", Colour.class "bg" q.colour, Colour.classUp "border" q.colour ] <|
        [ div [ class "flex justify-between items-end" ]
            [ Controls.textbox [ class "w-full" ] [ A.id <| "q-" ++ Trackable.idToString id ] q.question { isValid = True, isRequired = False } (QuestionTextUpdated id)
            ]
        , div [ class "flex justify-start items-end" ]
            [ Controls.textDropdown "mt-4 w-48 h-10 flex-shrink-0 flex-grow-0" (QuestionAnswerTypeUpdated id) answerTypeToString answerTypeFromString answerTypes (Just q.answerType) { showFilled = False }
            , Controls.colourDropdown "ml-auto flex-shrink-0 flex-grow-0" (QuestionColourUpdated id) (Just q.colour) { showFilled = False }
            ]
        ]
            ++ viewScaleOptions
            ++ viewIconOptions
            ++ [ Controls.button "mt-6 w-24" Controls.ButtonGrey (QuestionDeleteClicked id) SolidTrash "Delete" q.canDelete
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
