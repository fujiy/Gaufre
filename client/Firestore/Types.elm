module Firestore.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Id =
    String


type alias Path =
    Array Id


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


mapRemote : (a -> b) -> Remote a -> Remote b
mapRemote f r =
    case r of
        Loading ->
            Loading

        Failure ->
            Failure

        Committing a ->
            Committing (f a)

        UpToDate a ->
            UpToDate (f a)


appRemote : Remote (a -> b) -> Remote a -> Remote b
appRemote rf ra =
    case ( rf, ra ) of
        ( Failure, _ ) ->
            Failure

        ( _, Failure ) ->
            Failure

        ( Loading, _ ) ->
            Loading

        ( _, Loading ) ->
            Loading

        ( Committing f, Committing a ) ->
            Committing (f a)

        ( Committing f, UpToDate a ) ->
            Committing (f a)

        ( UpToDate f, Committing a ) ->
            Committing (f a)

        ( UpToDate f, UpToDate a ) ->
            UpToDate (f a)


root : Path
root =
    Array.empty


topLevel : Id -> Path
topLevel id =
    Array.fromList [ id ]


sub : Path -> Id -> Path
sub path id =
    Array.push id path



-- type alias Paths = Map ()
-- type Map a
--     = Map (Set a) (Dict Id (Map a))
-- empty : Map a
-- empty = Map Set.empty Dict.empty
-- singleton : Path -> comparable -> Map a
-- singleton path a =
--     Array.foldl
--         (\id m -> Map Set.empty <| Dict.singleton id m)
--         (Map (Set.singleton a) Dict.empty)
--         path
