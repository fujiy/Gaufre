module Firestore.Remote exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Util exposing (uncurry)


type Remote a
    = Loading
    | Failure
    | Committing a
    | UpToDate a


toMaybe : Remote a -> Maybe a
toMaybe r =
    case r of
        Failure ->
            Nothing

        Loading ->
            Nothing

        Committing a ->
            Just a

        UpToDate a ->
            Just a


fromMaybe : Maybe a -> Remote a
fromMaybe ma =
    case ma of
        Nothing ->
            Failure

        Just a ->
            UpToDate a


withDefault : a -> Remote a -> a
withDefault a =
    toMaybe >> Maybe.withDefault a


unwrap : b -> (a -> b) -> Remote a -> b
unwrap b f r =
    case toMaybe r of
        Nothing ->
            b

        Just a ->
            f a


map : (a -> b) -> Remote a -> Remote b
map f r =
    case r of
        Loading ->
            Loading

        Failure ->
            Failure

        Committing a ->
            Committing (f a)

        UpToDate a ->
            UpToDate (f a)


map2 : (a -> b -> c) -> Remote a -> Remote b -> Remote c
map2 f ra =
    app <| map f ra


map3 : (a -> b -> c -> d) -> Remote a -> Remote b -> Remote c -> Remote d
map3 f ra rb =
    app <| app (map f ra) rb


app : Remote (a -> b) -> Remote a -> Remote b
app rf ra =
    andThen (\f -> map f ra) rf


andThen : (a -> Remote b) -> Remote a -> Remote b
andThen f ra =
    case ra of
        Failure ->
            Failure

        Loading ->
            Loading

        Committing a ->
            case f a of
                UpToDate b ->
                    Committing b

                rb ->
                    rb

        UpToDate a ->
            f a


andThen2 : (a -> b -> Remote c) -> Remote a -> Remote b -> Remote c
andThen2 f ra rb =
    andThen (uncurry f) <|
        map2 Tuple.pair ra rb


unmaybe : Remote (Maybe a) -> Remote a
unmaybe =
    andThen fromMaybe


cats : List (Remote a) -> List a
cats =
    List.filterMap toMaybe


list : List (Remote a) -> Remote (List a)
list =
    List.foldr (map2 (::)) (UpToDate [])


traverse : ((a -> Remote a) -> t -> r) -> (Remote x -> r) -> Remote t -> r
traverse f default rx =
    case rx of
        Loading ->
            default Loading

        Failure ->
            default Failure

        Committing x ->
            f Committing x

        UpToDate x ->
            f UpToDate x


encode : (a -> Value) -> Remote a -> Value
encode enc ra =
    let
        ( status, v ) =
            case ra of
                Loading ->
                    ( "loading", Encode.null )

                Failure ->
                    ( "failure", Encode.null )

                Committing a ->
                    ( "committing", enc a )

                UpToDate a ->
                    ( "uptodate", enc a )
    in
    Encode.object
        [ ( "status", Encode.string status )
        , ( "value", v )
        ]


decode : Decoder a -> Decoder (Remote a)
decode dec =
    Decode.andThen
        (\t ->
            case t of
                ( "loading", _ ) ->
                    Decode.succeed Loading

                ( "failure", _ ) ->
                    Decode.succeed Failure

                ( "committing", Nothing ) ->
                    Decode.fail "no value"

                ( "committing", Just a ) ->
                    Decode.succeed <| Committing a

                ( "uptodate", Nothing ) ->
                    Decode.fail "no value"

                ( "uptodate", Just a ) ->
                    Decode.succeed <| UpToDate a

                _ ->
                    Decode.fail "unknown state"
        )
    <|
        Decode.map2 Tuple.pair
            (Decode.field "status" Decode.string)
            (Decode.field "value" <| Decode.nullable dec)
