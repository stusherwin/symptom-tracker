module UserData.Chartable exposing (Chartable(..), ChartableDict, Presentation, State, addTrackable, build, decode, deleteTrackable, encode, replaceTrackable, setColour, setInverted, setMultiplier, setName)

import Colour exposing (Colour)
import Dict exposing (Dict)
import Dictx
import IdDict exposing (IdDict(..))
import Json.Decode as D
import Json.Encode as E
import Listx
import Tuple
import UserData.ChartableId exposing (ChartableId)
import UserData.Trackable as Trackable exposing (Trackable, TrackableDict)
import UserData.TrackableId as TrackableId exposing (TrackableId)


type Chartable
    = Chartable State Presentation


type alias State =
    { name : String
    , colour : Maybe Colour
    , inverted : Bool
    , sum : List ( TrackableId, Float )
    }


type alias Presentation =
    { sum : List ( TrackableId, ( Trackable, Float ) )
    , colour : Colour
    , dataPoints : Dict Int Float
    }


type alias ChartableDict =
    IdDict ChartableId Chartable


build : TrackableDict -> State -> Chartable
build trackables state =
    let
        sum =
            trackableSum trackables state
    in
    Chartable state
        { sum = sum
        , colour = presentationColour sum state
        , dataPoints = dataPoints sum state
        }


trackableSum : TrackableDict -> State -> List ( TrackableId, ( Trackable, Float ) )
trackableSum trackables state =
    state.sum
        |> List.filterMap (\( id, m ) -> IdDict.get id trackables |> Maybe.map (\t -> ( id, ( t, m ) )))


presentationColour : List ( TrackableId, ( Trackable, Float ) ) -> State -> Colour
presentationColour sum state =
    let
        colourM =
            if List.length sum == 1 || state.colour == Nothing then
                List.head sum
                    |> Maybe.map (.colour << Tuple.first << Tuple.second)

            else
                state.colour
    in
    colourM |> Maybe.withDefault Colour.Gray


dataPoints : List ( TrackableId, ( Trackable, Float ) ) -> State -> Dict Int Float
dataPoints sum state =
    let
        invert data =
            case List.maximum <| Dict.values data of
                Just max ->
                    data |> Dict.map (\_ v -> max - v)

                _ ->
                    data
    in
    sum
        |> List.map
            (\( _, ( trackable, multiplier ) ) ->
                trackable
                    |> Trackable.onlyFloatData
                    |> Dict.map (\_ v -> v * multiplier)
            )
        |> List.foldl (Dictx.unionWith (\v1 v2 -> v1 + v2)) Dict.empty
        |> (if state.inverted then
                invert

            else
                identity
           )


setName : String -> Chartable -> Chartable
setName name (Chartable s p) =
    Chartable { s | name = name } p


setColour : Maybe Colour -> Chartable -> Chartable
setColour colour (Chartable s p) =
    let
        s_ =
            { s | colour = colour }
    in
    Chartable s_ { p | colour = presentationColour p.sum s_ }


setInverted : Bool -> Chartable -> Chartable
setInverted inverted (Chartable s p) =
    let
        s_ =
            { s | inverted = inverted }
    in
    Chartable s_ { p | dataPoints = dataPoints p.sum s_ }


addTrackable : TrackableId -> Trackable -> Float -> Chartable -> Chartable
addTrackable trackableId trackable multiplier (Chartable s p) =
    let
        s_ =
            { s | sum = s.sum ++ [ ( trackableId, multiplier ) ] }

        sum_ =
            p.sum ++ [ ( trackableId, ( trackable, multiplier ) ) ]
    in
    Chartable s_ { p | colour = presentationColour sum_ s_, dataPoints = dataPoints sum_ s_ }


deleteTrackable : TrackableId -> Chartable -> Chartable
deleteTrackable trackableId (Chartable s p) =
    let
        sSum_ =
            s.sum |> Listx.deleteBy Tuple.first trackableId

        pSum_ =
            p.sum |> Listx.deleteBy Tuple.first trackableId

        s_ =
            { s
                | sum = sSum_
                , colour =
                    if List.length sSum_ == 1 then
                        Nothing

                    else
                        s.colour
            }
    in
    Chartable
        s_
        { p
            | sum = pSum_
            , colour = presentationColour pSum_ s_
            , dataPoints = dataPoints pSum_ s_
        }


replaceTrackable : TrackableId -> TrackableId -> Trackable -> Chartable -> Chartable
replaceTrackable oldTrackableId newTrackableId newTrackable (Chartable s p) =
    let
        sSum_ =
            s.sum |> Listx.updateLookupWithKey oldTrackableId (\( _, multiplier ) -> ( newTrackableId, multiplier ))

        pSum_ =
            p.sum |> Listx.updateLookupWithKey oldTrackableId (\( _, ( _, multiplier ) ) -> ( newTrackableId, ( newTrackable, multiplier ) ))

        s_ =
            { s | sum = sSum_ }
    in
    Chartable s_
        { p
            | sum = pSum_
            , colour = presentationColour pSum_ s_
            , dataPoints = dataPoints pSum_ s_
        }


setMultiplier : TrackableId -> Float -> Chartable -> Chartable
setMultiplier trackableId multiplier (Chartable s p) =
    let
        sSum_ =
            s.sum |> Listx.updateLookup trackableId (always multiplier)

        pSum_ =
            p.sum |> Listx.updateLookup trackableId (Tuple.mapSecond <| always multiplier)

        s_ =
            { s | sum = sSum_ }
    in
    Chartable s_
        { p
            | sum = pSum_
            , dataPoints = dataPoints pSum_ s_
        }


decode : D.Decoder State
decode =
    D.map4
        (\name colour inverted sum ->
            { name = name
            , colour = colour
            , inverted = inverted
            , sum = sum
            }
        )
        (D.field "name" D.string)
        (D.field "colour" <| D.oneOf [ D.null Nothing, D.map Just Colour.decode ])
        (D.field "inverted" D.bool)
        (D.field "sum" <|
            D.list <|
                D.map2 Tuple.pair
                    (D.field "trackableId" TrackableId.decode)
                    (D.field "multiplier" D.float)
        )


encode : Chartable -> E.Value
encode (Chartable { name, colour, inverted, sum } _) =
    E.object
        [ ( "name", E.string name )
        , ( "colour"
          , case colour of
                Just c ->
                    Colour.encode c

                _ ->
                    E.null
          )
        , ( "inverted", E.bool inverted )
        , ( "sum"
          , E.list
                (\( trackableId, multiplier ) ->
                    E.object
                        [ ( "trackableId", TrackableId.encode trackableId )
                        , ( "multiplier", E.float multiplier )
                        ]
                )
                sum
          )
        ]
