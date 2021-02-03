module Firestore.Access exposing (..)

import Array exposing (Array)
import Dict
import Firestore.Internal as Internal exposing (..)
import Firestore.Types exposing (..)
import Util exposing (..)


type alias Accessor r =
    Internal.Accessor r


just : a -> Accessor a
just a =
    Accessor Array.empty (Just a)


nothing : Accessor a
nothing =
    Accessor Array.empty Nothing


map : (a -> b) -> Accessor a -> Accessor b
map f (Accessor paths ma) =
    Accessor paths <| Maybe.map f ma


map2 : (a -> b -> c) -> Accessor a -> Accessor b -> Accessor c
map2 f (Accessor aps ma) (Accessor bps mb) =
    Accessor (Array.append aps bps) <| Maybe.map2 f ma mb


map3 : (a -> b -> c -> d) -> Accessor a -> Accessor b -> Accessor c -> Accessor d
map3 f (Accessor aps ma) (Accessor bps mb) (Accessor cps mc) =
    Accessor (Array.append aps <| Array.append bps cps) <| Maybe.map3 f ma mb mc


andThen : (a -> Accessor b) -> Accessor a -> Accessor b
andThen f (Accessor paths ma) =
    case ma of
        Nothing ->
            Accessor paths Nothing

        Just a ->
            requires paths <| f a


for : (a -> Accessor b) -> Accessor (List a) -> Accessor (List b)
for f ac =
    map (List.map f >> list) ac |> andThen identity


list : List (Accessor a) -> Accessor (List a)
list =
    List.foldr (map2 (::)) (just [])


maybe : Accessor a -> Accessor (Maybe a)
maybe (Accessor paths ma) =
    Accessor paths <| Just ma


doc : Collection r -> Id -> Accessor r
doc col id =
    let
        (Accessor paths mr) =
            doc_ col id
    in
    Accessor paths <| Maybe.andThen toMaybe mr


doc_ :
    Collection r
    -> Id
    -> Accessor (Remote r)
doc_ (Collection { path, documents }) id =
    Accessor (singleton <| sub path id) <|
        Just <|
            case Dict.get id documents of
                Nothing ->
                    Loading

                Just (Document d) ->
                    d.data


collection : Collection r -> (List ( Id, r ) -> Accessor a) -> Accessor a
collection col f =
    collection_ col <|
        List.filterMap
            (\( id, rr ) -> toMaybe rr |> Maybe.map (Tuple.pair id))
            >> f


collection_ :
    Collection r
    -> (List ( Id, Remote r ) -> Accessor a)
    -> Accessor a
collection_ (Collection c) use =
    Dict.toList c.documents
        |> List.filterMap
            (\( id, Document d ) ->
                case d.data of
                    Failure ->
                        Nothing

                    _ ->
                        Just ( id, d.data )
            )
        |> use
        |> require c.path


get : Reference r -> Accessor r
get ref =
    let
        (Accessor paths mr) =
            get_ ref
    in
    Accessor paths <| Maybe.andThen toMaybe mr


get_ :
    Reference r
    -> Accessor (Remote r)
get_ (Reference ld) =
    let
        (Document d) =
            ld ()
    in
    Accessor (singleton d.path) <| Just d.data


require : Path -> Accessor a -> Accessor a
require path (Accessor paths ma) =
    Accessor (Array.push path paths) ma


requires : Array Path -> Accessor a -> Accessor a
requires paths_ (Accessor paths ma) =
    Accessor (Array.append paths_ paths) ma
