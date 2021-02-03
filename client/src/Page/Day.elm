module Page.Day exposing (Model, Msg(..), init, update, view)

import Array
import Button
import Colour exposing (Colour)
import Date exposing (Date, Unit(..))
import Dict exposing (Dict)
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Icon exposing (IconType(..), icon)
import Maybe exposing (Maybe)
import Task
import Textarea
import Textbox
import Time exposing (Month(..))
import Trackable exposing (Trackable, TrackableData(..))
import UserData exposing (UserData)


type alias Model =
    { currentDay : Date
    , today : Date
    , questions : Dict Int Question
    }


type alias Question =
    { question : String
    , colour : Colour
    , answer : Answer
    }


type Answer
    = AYesNo (Maybe Bool)
    | AIcon (List IconType) (Maybe Int)
    | AScale Int Int (Maybe Int)
    | AInt ( String, Maybe Int )
    | AFloat ( String, Maybe Float )
    | AText String


init : Date -> Date -> UserData -> Model
init today currentDay userData =
    { today = today
    , currentDay = currentDay
    , questions = Dict.map (toQuestion currentDay) <| UserData.trackables userData
    }


toQuestion : Date -> Int -> Trackable -> Question
toQuestion currentDay _ { question, colour, data } =
    { question = question
    , colour = colour
    , answer =
        case data of
            TYesNo answers ->
                AYesNo <| Dict.get (Date.toRataDie currentDay) answers

            TIcon options answers ->
                AIcon (Array.toList options) <| Dict.get (Date.toRataDie currentDay) answers

            TScale min max answers ->
                AScale min max <| Dict.get (Date.toRataDie currentDay) answers

            TInt answers ->
                let
                    answer =
                        Dict.get (Date.toRataDie currentDay) answers
                in
                AInt ( Maybe.withDefault "" <| Maybe.map String.fromInt answer, answer )

            TFloat answers ->
                let
                    answer =
                        Dict.get (Date.toRataDie currentDay) answers
                in
                AFloat ( Maybe.withDefault "" <| Maybe.map String.fromFloat answer, answer )

            TText answers ->
                AText <| Maybe.withDefault "" <| Dict.get (Date.toRataDie currentDay) answers
    }


updateQuestionFromTrackable : Date -> Trackable -> Question -> Question
updateQuestionFromTrackable _ { question, colour } q =
    { question = question
    , colour = colour
    , answer = q.answer
    }



-- UPDATE


type Msg
    = NoOp
    | QYesNoAnswerClicked Int (Maybe Bool)
    | QIconAnswerClicked Int (Maybe Int)
    | QScaleAnswerClicked Int (Maybe Int)
    | QIntAnswerUpdated Int String
    | QFloatAnswerUpdated Int String
    | QTextAnswerUpdated Int String
    | UpdateTrackable (Trackable -> Result String Trackable) Int
    | UserDataChanged UserData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateQuestion qId fn =
            { model | questions = Dict.update qId (Maybe.map fn) model.questions }
    in
    case msg of
        QYesNoAnswerClicked qId answer ->
            ( updateQuestion qId <|
                \q ->
                    case q.answer of
                        AYesNo _ ->
                            { q | answer = AYesNo answer }

                        _ ->
                            q
            , Task.perform (UpdateTrackable <| Trackable.updateYesNoData model.currentDay answer) <| Task.succeed qId
            )

        QIconAnswerClicked qId answer ->
            ( updateQuestion qId <|
                \q ->
                    case q.answer of
                        AIcon opts _ ->
                            { q | answer = AIcon opts answer }

                        _ ->
                            q
            , Task.perform (UpdateTrackable <| Trackable.updateIconData model.currentDay answer) <| Task.succeed qId
            )

        QScaleAnswerClicked qId answer ->
            ( updateQuestion qId <|
                \q ->
                    case q.answer of
                        AScale min max _ ->
                            { q | answer = AScale min max answer }

                        _ ->
                            q
            , Task.perform (UpdateTrackable <| Trackable.updateScaleData model.currentDay answer) <| Task.succeed qId
            )

        QIntAnswerUpdated qId ansStr ->
            let
                answer =
                    String.toInt ansStr
            in
            ( updateQuestion qId <|
                \q ->
                    case q.answer of
                        AInt _ ->
                            { q | answer = AInt ( ansStr, answer ) }

                        _ ->
                            q
            , case ( ansStr, answer ) of
                ( "", _ ) ->
                    Task.perform (UpdateTrackable <| Trackable.updateIntData model.currentDay Nothing) <| Task.succeed qId

                ( _, Just ans ) ->
                    Task.perform (UpdateTrackable <| Trackable.updateIntData model.currentDay (Just ans)) <| Task.succeed qId

                _ ->
                    Cmd.none
            )

        QFloatAnswerUpdated qId ansStr ->
            let
                answer =
                    String.toFloat ansStr
            in
            ( updateQuestion qId <|
                \q ->
                    case q.answer of
                        AFloat _ ->
                            { q | answer = AFloat ( ansStr, answer ) }

                        _ ->
                            q
            , case ( ansStr, answer ) of
                ( "", _ ) ->
                    Task.perform (UpdateTrackable <| Trackable.updateFloatData model.currentDay Nothing) <| Task.succeed qId

                ( _, Just ans ) ->
                    Task.perform (UpdateTrackable <| Trackable.updateFloatData model.currentDay (Just ans)) <| Task.succeed qId

                _ ->
                    Cmd.none
            )

        QTextAnswerUpdated qId answer ->
            ( updateQuestion qId <|
                \q ->
                    case q.answer of
                        AText _ ->
                            { q | answer = AText answer }

                        _ ->
                            q
            , Task.perform (UpdateTrackable <| Trackable.updateTextData model.currentDay answer) <| Task.succeed qId
            )

        UserDataChanged userData ->
            let
                onlyInTrackables k t qs =
                    Dict.insert k (toQuestion model.currentDay k t) qs

                inBoth k t q qs =
                    Dict.insert k (updateQuestionFromTrackable model.currentDay t q) qs

                onlyInQuestions k v qs =
                    Dict.insert k v qs

                questions =
                    Dict.merge onlyInTrackables inBoth onlyInQuestions (UserData.trackables userData) model.questions Dict.empty
            in
            ( { model | questions = questions }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { currentDay, today, questions } =
    let
        dayText =
            if currentDay == today then
                "today"

            else if Date.diff Days today currentDay == -1 then
                "yesterday"

            else if Date.diff Days today currentDay > -7 then
                Date.format "EEEE" currentDay

            else if Date.diff Years today currentDay == 0 then
                Date.format "EEE d MMMM" currentDay

            else
                Date.format "d MMMM y" currentDay
    in
    div [] <|
        [ viewDayPicker currentDay today
        , h2 [ class "py-4 font-bold text-2xl text-center shadow-inner-t-md" ]
            [ text <| "How was " ++ dayText ++ "?" ]
        ]
            ++ Dict.values (Dict.map viewQuestion questions)


viewDayPicker : Date -> Date -> Html Msg
viewDayPicker currentDay today =
    let
        link date =
            if date == today then
                "/"

            else
                "/day/" ++ Date.format "y/M/d" date

        dateButtons =
            let
                currentWeekday =
                    Date.weekdayNumber currentDay

                start =
                    Date.add Days (0 - (currentWeekday - 1)) currentDay

                dateButton date =
                    if Date.diff Days today date > 0 then
                        div [ class "w-12 h-12 flex-grow-0 flex-shrink-0 bg-gray-200" ]
                            [ div [ class "flex flex-col items-center justify-center h-full" ]
                                [ span [ class "text-xs text-gray-400 leading-none whitespace-nowrap uppercase" ] [ text (Date.format "EEE" date) ]
                                , span [ class "mt-px text-xl text-gray-400 leading-none whitespace-nowrap uppercase font-medium" ] [ text (Date.format "d" date) ]
                                ]
                            ]

                    else if date == currentDay then
                        div [ class "w-12 h-12 flex-grow-0 flex-shrink-0 bg-white shadow-inner" ]
                            [ div [ class "flex flex-col items-center justify-center h-full" ]
                                [ span [ class "text-xs text-gray-700 leading-none whitespace-nowrap uppercase" ] [ text (Date.format "EEE" date) ]
                                , span [ class "mt-px text-xl leading-none whitespace-nowrap uppercase font-medium" ] [ text (Date.format "d" date) ]
                                ]
                            ]

                    else
                        a
                            [ href (link date)
                            , class "w-12 h-12 flex-grow-0 flex-shrink-0 bg-gray-200 hover:bg-gray-50 hover:shadow-inner"
                            ]
                            [ div [ class "flex flex-col items-center justify-center h-full" ]
                                [ span [ class "text-xs text-gray-700 leading-none whitespace-nowrap uppercase" ] [ text (Date.format "EEE" date) ]
                                , span [ class "mt-px text-xl leading-none whitespace-nowrap uppercase font-medium" ] [ text (Date.format "d" date) ]
                                ]
                            ]
            in
            div [ class "flex bg-gray-100 border-2 border-gray-300 divide-x-2 divide-gray-300 normal-nums" ] <|
                List.map (\d -> dateButton (Date.add Days d start)) (List.range 0 6)

        arrowButton iconType activeClass inactiveClass date =
            if date == currentDay || Date.diff Days today date > 0 then
                div
                    [ class "flex justify-center items-center"
                    , class inactiveClass
                    ]
                    [ icon "w-4 h-4" iconType ]

            else
                a
                    [ href (link date)
                    , class "flex justify-center items-center"
                    , class activeClass
                    ]
                    [ icon "w-4 h-4" iconType ]
    in
    div [ class "w-full bg-gray-200 border-b-4 border-gray-300 p-2 flex justify-center items-center" ]
        [ div [ class "flex flex-col" ]
            [ dateButtons
            , div [ class "mt-2 flex justify-between items-center" ]
                [ arrowButton SolidAngleDoubleLeft
                    "w-6 h-6 rounded bg-gray-800 border-2 border-gray-900 text-white hover:bg-gray-700 hover:border-gray-800"
                    "w-6 h-6 rounded bg-gray-400 border-2 border-gray-400 text-white"
                    (Date.add Weeks -1 currentDay)
                , arrowButton SolidAngleLeft
                    "w-6 h-6 rounded bg-gray-800 border-2 border-gray-900 text-white hover:bg-gray-700 hover:border-gray-800"
                    "w-6 h-6 rounded bg-gray-400 border-2 border-gray-400 text-white"
                    (Date.add Days -1 currentDay)
                , div [ class "flex justify-center items-center rounded overflow-hidden border-2 border-gray-300 divide-x-2 divide-gray-300" ]
                    [ arrowButton SolidAngleLeft "w-6 h-6 text-gray-900 hover:bg-gray-300" "w-6 h-6 text-gray-400" (Date.add Months -1 currentDay)
                    , div [ class "w-12 h-6 py-1 bg-white shadow-inner text-xs uppercase text-center leading-none whitespace-nowrap flex flex-col justify-center" ]
                        [ span [] [ text (Date.format "MMM" currentDay) ] ]
                    , arrowButton SolidAngleRight "w-6 h-6 text-gray-900 hover:bg-gray-300" "w-6 h-6 text-gray-400" (Date.add Months 1 currentDay)
                    ]
                , div [ class "flex justify-center items-center rounded overflow-hidden border-2 border-gray-300 divide-x-2 divide-gray-300" ]
                    [ arrowButton SolidAngleLeft "w-6 h-6 text-gray-900 hover:bg-gray-300" "w-6 h-6 text-gray-400" (Date.add Years -1 currentDay)
                    , div [ class "w-12 h-6 py-1 bg-white shadow-inner text-base uppercase text-center leading-none whitespace-nowrap flex flex-col justify-center" ]
                        [ span [] [ text (Date.format "y" currentDay) ] ]
                    , arrowButton SolidAngleRight "w-6 h-6 text-gray-900 hover:bg-gray-300" "w-6 h-6 text-gray-400" (Date.add Years 1 currentDay)
                    ]
                , arrowButton SolidAngleRight
                    "w-6 h-6 rounded bg-gray-800 border-2 border-gray-900 text-white hover:bg-gray-700 hover:border-gray-800"
                    "w-6 h-6 rounded bg-gray-400 border-2 border-gray-400 text-white"
                    (Date.add Days 1 currentDay)
                , arrowButton SolidAngleDoubleRight
                    "w-6 h-6 rounded bg-gray-800 border-2 border-gray-900 text-white hover:bg-gray-700 hover:border-gray-800"
                    "w-6 h-6 rounded bg-gray-400 border-2 border-gray-400 text-white"
                    (Date.add Weeks 1 currentDay)
                ]
            ]
        ]


viewQuestion : Int -> Question -> Html Msg
viewQuestion qId { question, colour, answer } =
    let
        viewAnswer =
            case answer of
                AIcon options value ->
                    viewIconAnswer qId options value

                AYesNo value ->
                    viewYesNoAnswer qId value

                AScale min max value ->
                    if max - min + 1 <= 10 then
                        viewScaleAnswerButtons min max qId value

                    else
                        viewScaleAnswerSelect min max qId value

                AInt ( stringValue, value ) ->
                    let
                        isValid =
                            case ( stringValue, value ) of
                                ( "", _ ) ->
                                    True

                                ( _, Just _ ) ->
                                    True

                                _ ->
                                    False
                    in
                    viewIntAnswer qId stringValue isValid

                AFloat ( stringValue, value ) ->
                    let
                        isValid =
                            case ( stringValue, value ) of
                                ( "", _ ) ->
                                    True

                                ( _, Just _ ) ->
                                    True

                                _ ->
                                    False
                    in
                    viewFloatAnswer qId stringValue isValid

                AText value ->
                    viewTextAnswer qId <| value
    in
    div [ class "pt-6 pb-6 border-t-4", Colour.class "bg" colour, Colour.classUp "border" colour ]
        [ h2 [ class "font-bold text-xl text-center" ] [ text question ]
        , div [ class "flex justify-center" ] viewAnswer
        ]


viewIconAnswer : Int -> List IconType -> Maybe Int -> List (Html Msg)
viewIconAnswer qId options answer =
    let
        iconButton : Int -> IconType -> Html Msg
        iconButton value iconType =
            Button.viewIcon
                ("mt-4 mr-4 last:mr-0 "
                    ++ (if answer == Just value then
                            "btn-blue"

                        else
                            "btn-gray"
                       )
                )
                (QIconAnswerClicked qId <|
                    if answer == Just value then
                        Nothing

                    else
                        Just value
                )
                iconType
    in
    List.indexedMap iconButton options


viewYesNoAnswer : Int -> Maybe Bool -> List (Html Msg)
viewYesNoAnswer qId answer =
    let
        buttonText value =
            if value then
                "Yes"

            else
                "No"

        buttonIcon value =
            if value then
                SolidCheckCircle

            else
                SolidTimesCircle

        yesNoButton : Bool -> Html Msg
        yesNoButton value =
            Button.view
                "mt-4 mr-2 last:mr-0"
                (if answer == Just value then
                    Button.Blue

                 else
                    Button.Grey
                )
                (QYesNoAnswerClicked qId <|
                    if answer == Just value then
                        Nothing

                    else
                        Just value
                )
                (buttonIcon value)
                (buttonText value)
                True
    in
    [ yesNoButton False
    , yesNoButton True
    ]


viewScaleAnswerButtons : Int -> Int -> Int -> Maybe Int -> List (Html Msg)
viewScaleAnswerButtons min max qId answer =
    let
        scaleButton : Int -> Html Msg
        scaleButton level =
            Button.viewCircle
                ("mt-4 mr-4 last:mr-0 "
                    ++ (if answer == Just level then
                            "btn-blue"

                        else
                            "btn-gray"
                       )
                )
                (QScaleAnswerClicked qId <|
                    if answer == Just level then
                        Nothing

                    else
                        Just level
                )
                (String.fromInt level)
    in
    List.map scaleButton <|
        List.range min max


viewScaleAnswerSelect : Int -> Int -> Int -> Maybe Int -> List (Html Msg)
viewScaleAnswerSelect min max qId answer =
    [ Dropdown.viewText
        "mt-4 w-20"
        (QScaleAnswerClicked qId)
        String.fromInt
        String.toInt
        (List.map (\i -> ( ( i, True ), String.fromInt i )) <| List.range min max)
        answer
        { showFilled = True }
    ]


viewIntAnswer : Int -> String -> Bool -> List (Html Msg)
viewIntAnswer qId answer isValid =
    [ Textbox.view "" "mt-4 w-20" answer (QIntAnswerUpdated qId) isValid { showFilled = True }
    ]


viewFloatAnswer : Int -> String -> Bool -> List (Html Msg)
viewFloatAnswer qId answer isValid =
    [ Textbox.view "" "mt-4 w-20" answer (QFloatAnswerUpdated qId) isValid { showFilled = True }
    ]


viewTextAnswer : Int -> String -> List (Html Msg)
viewTextAnswer qId answer =
    [ Textarea.view "" "mt-4 w-96 h-36" answer (QTextAnswerUpdated qId) True { showFilled = True }
    ]
