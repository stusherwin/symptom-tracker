module Page.Day exposing (Model, Msg(..), init, update, view)

import Array exposing (Array)
import Colour
import Controls
import Date exposing (Date, Unit(..))
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import IdDict exposing (IdDict)
import Maybe exposing (Maybe)
import Svg.Icon exposing (IconType(..), icon)
import Task
import Time exposing (Month(..))
import UserData exposing (UserData)
import UserData.Trackable as T exposing (Responses(..), Trackable)
import UserData.TrackableId as TId exposing (TrackableId)


type alias Model =
    { currentDay : Date
    , today : Date
    , trackables : List ( TrackableId, ( Trackable, Bool ) )
    , textInputs : IdDict TrackableId ( String, Bool )
    }


init : Date -> Date -> UserData -> Model
init today currentDay userData =
    { today = today
    , currentDay = currentDay
    , trackables = UserData.activeTrackables userData
    , textInputs =
        IdDict.map (\_ v -> ( v, True )) <|
            IdDict.map
                (\_ t ->
                    Maybe.withDefault "" <|
                        Dict.get (Date.toRataDie currentDay) <|
                            T.textData t
                )
            <|
                TId.toDict <|
                    List.map (\( id, ( t, _ ) ) -> ( id, t )) <|
                        List.filter (\( _, ( _, visible ) ) -> visible) <|
                            UserData.activeTrackables userData
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
    | UserDataUpdated UserData


update : UserData -> Msg -> Model -> ( Model, Cmd Msg )
update userData msg model =
    let
        updateUserData fn ( m, cmd ) =
            let
                userData_ =
                    userData |> fn
            in
            ( { m | trackables = UserData.activeTrackables userData_ }
            , Cmd.batch [ cmd, Task.perform UserDataUpdated <| Task.succeed userData_ ]
            )

        when cond fn =
            if cond then
                fn

            else
                identity
    in
    case msg of
        YesNoAnswerClicked id answer ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.updateYesNoData model.currentDay answer)

        IconAnswerClicked id answer ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.updateIconData model.currentDay answer)

        ScaleAnswerClicked id answer ->
            ( model, Cmd.none )
                |> (updateUserData <| UserData.updateTrackable id <| T.updateScaleData model.currentDay answer)

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
            ( model, Cmd.none )
                |> (Tuple.mapFirst <| \m -> { m | textInputs = textInputs })
                |> (when isValid <| updateUserData <| UserData.updateTrackable id <| T.updateIntData model.currentDay answer)

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
            ( model, Cmd.none )
                |> (Tuple.mapFirst <| \m -> { m | textInputs = textInputs })
                |> (when isValid <| updateUserData <| UserData.updateTrackable id <| T.updateFloatData model.currentDay answer)

        TextAnswerUpdated id answer ->
            ( model, Cmd.none )
                |> (Tuple.mapFirst <| \m -> { m | textInputs = IdDict.insert id ( answer, True ) m.textInputs })
                |> (updateUserData <| UserData.updateTrackable id <| T.updateTextData model.currentDay answer)

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { currentDay, today, trackables, textInputs } =
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
        , h2 [ class "py-4 font-bold text-2xl text-center bg-white" ]
            [ text <| "How was " ++ dayText ++ "?" ]
        ]
            ++ (List.map (viewQuestion currentDay textInputs) <|
                    List.map (\( id, ( t, _ ) ) -> ( id, t )) <|
                        List.filter (\( _, ( _, visible ) ) -> visible) <|
                            trackables
               )


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
                        div [ class "w-12 h-12 first:rounded-l last:rounded-r flex-grow-0 flex-shrink-0 bg-gray-300" ]
                            [ div [ class "flex flex-col items-center justify-center h-full" ]
                                [ span [ class "text-xs text-gray-500 leading-none whitespace-nowrap uppercase" ] [ text (Date.format "EEE" date) ]
                                , span [ class "mt-px text-xl text-gray-500 leading-none whitespace-nowrap uppercase font-medium" ] [ text (Date.format "d" date) ]
                                ]
                            ]

                    else if date == currentDay then
                        div [ class "w-12 h-12 first:rounded-l last:rounded-r flex-grow-0 flex-shrink-0 bg-white shadow-inner" ]
                            [ div [ class "flex flex-col items-center justify-center h-full" ]
                                [ span [ class "text-xs text-gray-700 leading-none whitespace-nowrap uppercase" ] [ text (Date.format "EEE" date) ]
                                , span [ class "mt-px text-xl leading-none whitespace-nowrap uppercase font-medium" ] [ text (Date.format "d" date) ]
                                ]
                            ]

                    else
                        a
                            [ href (link date)
                            , class "w-12 h-12 first:rounded-l last:rounded-r flex-grow-0 flex-shrink-0 bg-gray-200 hover:bg-gray-50 hover:shadow-inner"
                            ]
                            [ div [ class "flex flex-col items-center justify-center h-full" ]
                                [ span [ class "text-xs text-gray-700 leading-none whitespace-nowrap uppercase" ] [ text (Date.format "EEE" date) ]
                                , span [ class "mt-px text-xl leading-none whitespace-nowrap uppercase font-medium" ] [ text (Date.format "d" date) ]
                                ]
                            ]
            in
            div [ class "flex rounded border-2 border-gray-400 divide-x-2 divide-gray-400 normal-nums" ] <|
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
    div [ class "w-full bg-gray-300 border-b-4 border-gray-400 p-2 flex justify-center items-center" ]
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
                , div [ class "flex justify-center items-center rounded overflow-hidden border-2 border-gray-400 divide-x-2 divide-gray-400" ]
                    [ arrowButton SolidAngleLeft "w-6 h-6 text-gray-900 hover:bg-gray-300" "w-6 h-6 text-gray-400" (Date.add Months -1 currentDay)
                    , div [ class "w-12 h-6 py-1 bg-white shadow-inner text-xs uppercase text-center leading-none whitespace-nowrap flex flex-col justify-center" ]
                        [ span [] [ text (Date.format "MMM" currentDay) ] ]
                    , arrowButton SolidAngleRight "w-6 h-6 text-gray-900 hover:bg-gray-300" "w-6 h-6 text-gray-400" (Date.add Months 1 currentDay)
                    ]
                , div [ class "flex justify-center items-center rounded overflow-hidden border-2 border-gray-400 divide-x-2 divide-gray-400" ]
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


viewQuestion : Date -> IdDict TrackableId ( String, Bool ) -> ( TrackableId, Trackable ) -> Html Msg
viewQuestion currentDay textInputs ( id, t ) =
    let
        key =
            Date.toRataDie currentDay

        ( stringValue, isValid ) =
            Maybe.withDefault ( "", True ) <| IdDict.get id textInputs

        viewAnswer =
            case T.responses t of
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
    div [ class "pt-6 pb-6 border-t-4", Colour.bgClass (T.colour t), Colour.borderClassDarker (T.colour t) ]
        [ h2 [ class "font-bold text-xl text-center" ] [ text (T.question t) ]
        , div [ class "flex justify-center" ] viewAnswer
        ]


viewIconAnswer : TrackableId -> Array IconType -> Maybe Int -> List (Html Msg)
viewIconAnswer id options answer =
    let
        iconButton : Int -> IconType -> Html Msg
        iconButton value iconType =
            Controls.iconButton
                "mt-4 mr-4 last:mr-0 "
                (if answer == Just value then
                    Controls.ButtonBlue

                 else
                    Controls.ButtonGrey
                )
                (IconAnswerClicked id <|
                    if answer == Just value then
                        Nothing

                    else
                        Just value
                )
                iconType
                True
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
            Controls.button
                "mt-4 mr-2 last:mr-0"
                (if answer == Just value then
                    Controls.ButtonBlue

                 else
                    Controls.ButtonGrey
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
            Controls.circleButton
                "mt-4 mr-4 last:mr-0"
                (if answer == Just level then
                    Controls.ButtonBlue

                 else
                    Controls.ButtonGrey
                )
                (ScaleAnswerClicked id <|
                    if answer == Just level then
                        Nothing

                    else
                        Just level
                )
                (String.fromInt level)
                True
    in
    List.map scaleButton <|
        List.range min max


viewScaleAnswerSelect : Int -> Int -> TrackableId -> Maybe Int -> List (Html Msg)
viewScaleAnswerSelect min max id answer =
    [ Controls.textDropdown
        "mt-4 w-20 h-10"
        (ScaleAnswerClicked id)
        String.fromInt
        String.toInt
        (List.map (\i -> ( ( i, True ), String.fromInt i )) <| List.range min max)
        (Just "\u{00A0}")
        answer
        { showFilled = True }
    ]


viewIntAnswer : TrackableId -> String -> Bool -> List (Html Msg)
viewIntAnswer id answer isValid =
    [ Controls.textbox [ class "mt-4 w-20" ] [] answer { isValid = isValid, isRequired = False, isPristine = False } (IntAnswerUpdated id)
    ]


viewFloatAnswer : TrackableId -> String -> Bool -> List (Html Msg)
viewFloatAnswer id answer isValid =
    [ Controls.textbox [ class "mt-4 w-20" ] [] answer { isValid = isValid, isRequired = False, isPristine = False } (FloatAnswerUpdated id)
    ]


viewTextAnswer : TrackableId -> String -> List (Html Msg)
viewTextAnswer id answer =
    [ Controls.textarea "" "mt-4 w-96 h-36" answer (TextAnswerUpdated id) True { showFilled = True }
    ]
