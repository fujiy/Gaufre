module Firestore.Remote exposing (..)

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
app =
    map2 identity


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
