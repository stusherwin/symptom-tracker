module Page.Day exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Colour
import Control.Button as Button
import Control.Dropdown as Dropdown
import Control.Textarea as Textarea
import Control.Textbox as Textbox
import Date exposing (Date, Unit(..))
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import IdDict exposing (IdDict)
import Maybe exposing (Maybe)
import Result
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Trackable as Trackable exposing (Trackable, TrackableData(..), TrackableId)


type alias Model =
    { currentDay : Date
    , today : Date
    , userData : UserData
    , textInputs : IdDict TrackableId ( String, Bool )
    }


init : Date -> Date -> UserData -> Model
init today currentDay userData =
    { today = today
    , currentDay = currentDay
    , userData = userData
    , textInputs =
        IdDict.map (\_ v -> ( v, True )) <|
            IdDict.map
                (\_ t ->
                    Maybe.withDefault "" <|
                        Dict.get (Date.toRataDie currentDay) <|
                            Trackable.textData t
                )
            <|
                UserData.trackables userData
    }



-- UPDATE


type Msg
    = NoOp
    | YesNoAnswerClicked TrackableId (Maybe Bool)
    | IconAnswerClicked TrackableId (Maybe Int)
    | ScaleAnswerClicked TrackableId (Maybe Int)
    | IntAnswerUpdated TrackableId String
    | FloatAnswerUpdated TrackableId String
    | TextAnswerUpdated TrackableId String
    | UpdateUserData (Result String UserData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        YesNoAnswerClicked id answer ->
            let
                userData =
                    model.userData |> UserData.tryUpdateTrackable id (Trackable.updateYesNoData model.currentDay answer)
            in
            ( { model | userData = Result.withDefault model.userData userData }
            , Task.perform UpdateUserData <| Task.succeed userData
            )

        IconAnswerClicked id answer ->
            let
                userData =
                    model.userData |> UserData.tryUpdateTrackable id (Trackable.updateIconData model.currentDay answer)
            in
            ( { model | userData = Result.withDefault model.userData userData }
            , Task.perform UpdateUserData <| Task.succeed userData
            )

        ScaleAnswerClicked id answer ->
            let
                userData =
                    model.userData |> UserData.tryUpdateTrackable id (Trackable.updateScaleData model.currentDay answer)
            in
            ( { model | userData = Result.withDefault model.userData userData }
            , Task.perform UpdateUserData <| Task.succeed userData
            )

        IntAnswerUpdated id stringValue ->
            let
                answer =
                    String.toInt stringValue

                isValid =
                    case answer of
                        Just _ ->
                            True

                        _ ->
                            stringValue == ""

                textInputs =
                    IdDict.insert id ( stringValue, isValid ) model.textInputs
            in
            if isValid then
                let
                    userData =
                        model.userData |> UserData.tryUpdateTrackable id (Trackable.updateIntData model.currentDay answer)
                in
                ( { model | textInputs = textInputs, userData = Result.withDefault model.userData userData }
                , Task.perform UpdateUserData <| Task.succeed userData
                )

            else
                ( { model | textInputs = textInputs }, Cmd.none )

        FloatAnswerUpdated id stringValue ->
            let
                answer =
                    String.toFloat stringValue

                isValid =
                    case answer of
                        Just _ ->
                            True

                        _ ->
                            stringValue == ""

                textInputs =
                    IdDict.insert id ( stringValue, isValid ) model.textInputs
            in
            if isValid then
                let
                    userData =
                        model.userData |> UserData.tryUpdateTrackable id (Trackable.updateFloatData model.currentDay answer)
                in
                ( { model | textInputs = textInputs, userData = Result.withDefault model.userData userData }
                , Task.perform UpdateUserData <| Task.succeed userData
                )

            else
                ( { model | textInputs = textInputs }, Cmd.none )

        TextAnswerUpdated id answer ->
            let
                userData =
                    model.userData |> UserData.tryUpdateTrackable id (Trackable.updateTextData model.currentDay answer)
            in
            ( { model
                | textInputs = IdDict.insert id ( answer, True ) model.textInputs
                , userData = Result.withDefault model.userData userData
              }
            , Task.perform UpdateUserData <| Task.succeed userData
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { currentDay, today, userData, textInputs } =
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
            ++ IdDict.values (IdDict.map (viewQuestion currentDay textInputs) <| UserData.trackables userData)


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


viewQuestion : Date -> IdDict TrackableId ( String, Bool ) -> TrackableId -> Trackable -> Html Msg
viewQuestion currentDay textInputs id { question, colour, data } =
    let
        key =
            Date.toRataDie currentDay

        ( stringValue, isValid ) =
            Maybe.withDefault ( "", True ) <| IdDict.get id textInputs

        viewAnswer =
            case data of
                TIcon options values ->
                    viewIconAnswer id options <| Dict.get key values

                TYesNo values ->
                    viewYesNoAnswer id <| Dict.get key values

                TScale min max values ->
                    if max - min + 1 <= 10 then
                        viewScaleAnswerButtons min max id <| Dict.get key values

                    else
                        viewScaleAnswerSelect min max id <| Dict.get key values

                TInt _ ->
                    viewIntAnswer id stringValue isValid

                TFloat _ ->
                    viewFloatAnswer id stringValue isValid

                TText _ ->
                    viewTextAnswer id stringValue
    in
    div [ class "pt-6 pb-6 border-t-4", Colour.class "bg" colour, Colour.classUp "border" colour ]
        [ h2 [ class "font-bold text-xl text-center" ] [ text question ]
        , div [ class "flex justify-center" ] viewAnswer
        ]


viewIconAnswer : TrackableId -> Array IconType -> Maybe Int -> List (Html Msg)
viewIconAnswer id options answer =
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
                (IconAnswerClicked id <|
                    if answer == Just value then
                        Nothing

                    else
                        Just value
                )
                iconType
    in
    List.indexedMap iconButton <| Array.toList options


viewYesNoAnswer : TrackableId -> Maybe Bool -> List (Html Msg)
viewYesNoAnswer id answer =
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
                (YesNoAnswerClicked id <|
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


viewScaleAnswerButtons : Int -> Int -> TrackableId -> Maybe Int -> List (Html Msg)
viewScaleAnswerButtons min max id answer =
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
                (ScaleAnswerClicked id <|
                    if answer == Just level then
                        Nothing

                    else
                        Just level
                )
                (String.fromInt level)
    in
    List.map scaleButton <|
        List.range min max


viewScaleAnswerSelect : Int -> Int -> TrackableId -> Maybe Int -> List (Html Msg)
viewScaleAnswerSelect min max id answer =
    [ Dropdown.viewText
        "mt-4 w-20"
        (ScaleAnswerClicked id)
        String.fromInt
        String.toInt
        (List.map (\i -> ( ( i, True ), String.fromInt i )) <| List.range min max)
        answer
        { showFilled = True }
    ]


viewIntAnswer : TrackableId -> String -> Bool -> List (Html Msg)
viewIntAnswer id answer isValid =
    [ Textbox.view "" "mt-4 w-20" answer (IntAnswerUpdated id) isValid { showFilled = True }
    ]


viewFloatAnswer : TrackableId -> String -> Bool -> List (Html Msg)
viewFloatAnswer id answer isValid =
    [ Textbox.view "" "mt-4 w-20" answer (FloatAnswerUpdated id) isValid { showFilled = True }
    ]


viewTextAnswer : TrackableId -> String -> List (Html Msg)
viewTextAnswer id answer =
    [ Textarea.view "" "mt-4 w-96 h-36" answer (TextAnswerUpdated id) True { showFilled = True }
    ]
