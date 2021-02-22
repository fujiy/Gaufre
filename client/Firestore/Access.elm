module Firestore.Access exposing (..)

import Array exposing (Array)
import Dict
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path exposing (Id, Path, Paths)
import Firestore.Remote as Remote exposing (Remote(..))
import Maybe.Extra as Maybe
import Util exposing (..)


type alias Accessor r a =
    Internal.Accessor r a


success : a -> Accessor r a
success a =
    Accessor Path.empty (UpToDate a)


failure : Accessor r a
failure =
    Accessor Path.empty Failure


map : (a -> b) -> Accessor r a -> Accessor r b
map f (Accessor paths ra) =
    Accessor paths <| Remote.map f ra


map2 : (a -> b -> c) -> Accessor r a -> Accessor r b -> Accessor r c
map2 f (Accessor aps ra) (Accessor bps rb) =
    Accessor (Path.append aps bps) <| Remote.map2 f ra rb


map3 :
    (a -> b -> c -> d)
    -> Accessor r a
    -> Accessor r b
    -> Accessor r c
    -> Accessor r d
map3 f (Accessor aps ra) (Accessor bps rb) (Accessor cps rc) =
    Accessor (Path.append aps <| Path.append bps cps) <| Remote.map3 f ra rb rc


mapRemote : (Remote a -> Remote b) -> Accessor r a -> Accessor r b
mapRemote f (Accessor paths ra) =
    Accessor paths <| f ra


unremote : Remote (Accessor r a) -> Accessor r a
unremote ra =
    case ra of
        Failure ->
            failure

        Loading ->
            Accessor Path.empty Loading

        Committing (Accessor paths (UpToDate a)) ->
            Accessor paths (Committing a)

        Committing aa ->
            aa

        UpToDate aa ->
            aa


remote : Accessor r a -> Remote (Accessor r a)
remote (Accessor paths ra) =
    case ra of
        Failure ->
            Failure

        Loading ->
            Loading

        Committing a ->
            Committing <| Accessor paths <| UpToDate a

        UpToDate a ->
            UpToDate <| Accessor paths <| UpToDate a


andThen : (a -> Accessor r b) -> Accessor r a -> Accessor r b
andThen f (Accessor paths ra) =
    Remote.andThen (f >> remote) ra
        |> unremote
        |> requires paths


andThen2 : (a -> b -> Accessor r c) -> Accessor r a -> Accessor r b -> Accessor r c
andThen2 f (Accessor pas ra) (Accessor pbs rb) =
    let
        paths =
            Path.append pas pbs
    in
    Remote.andThen2 (\a b -> f a b |> remote) ra rb
        |> unremote
        |> requires (Path.append pas pbs)


for : (a -> Accessor r b) -> Accessor r (List a) -> Accessor r (List b)
for f ac =
    map (List.map f >> list) ac |> andThen identity


forArray : (a -> Accessor r b) -> Accessor r (Array a) -> Accessor r (Array b)
forArray f ac =
    map (Array.map f >> array) ac |> andThen identity


list : List (Accessor r a) -> Accessor r (List a)
list =
    List.foldr (map2 (::)) (success [])


array : Array (Accessor r a) -> Accessor r (Array a)
array =
    Array.toList >> list >> map Array.fromList



-- maybe : Maybe (Accessor r a) -> Accessor (Maybe a)
-- maybe m =
--     case m of
--         Nothing ->
--             failure
--         Just (Accessor paths ra) ->
--             Accessor paths <| UpToDate ra


maybe : Accessor r a -> Accessor r (Maybe a)
maybe =
    mapRemote <| Remote.toMaybe >> UpToDate


fromJust : Accessor r (Maybe a) -> Accessor r a
fromJust =
    mapRemote Remote.unmaybe



-- doc :
--     Collection r
--     -> Id
--     -> Accessor r
-- doc (Collection { path, documents }) id =
--     Accessor (Array.fromList [ Path.sub path id ]) <|
--         Just <|
--             case Dict.get id documents of
--                 Nothing ->
--                     Loading
--                 Just (Document d) ->
--                     d.data
-- doc x =
--     Debug.todo "doc"
-- collection : Collection r -> (List ( Id, r ) -> Accessor r a) -> Accessor r a
-- collection col f =
--     collection_ col <|
--         List.filterMap
--             (\( id, rr ) -> Remote.toMaybe rr |> Maybe.map (Tuple.pair id))
--             >> f
-- collection_ =
--     Debug.todo "collection"
-- collection_ :
--     Collection r
--     -> (List ( Id, Remote r ) -> Accessor r a)
--     -> Accessor r a
-- collection_ (Collection c) use =
-- r
-- Dict.toList c.documents
--         |> List.filterMap
--             (\( id, Document d ) ->
--                 case d.data of
--                     Failure ->
--                         Nothing
--                     _ ->
--                         Just ( id, d.data )
--             )
--         |> use
--         |> require c.path


access : Lens a r -> a -> Accessor a r
access (Lens acc _) a =
    acc a



-- let
--     (Accessor paths rr) =
--         acc a
-- in
-- Accessor paths <| Remote.andThen (\(Document r) -> r) rr
-- get : Reference r -> Accessor r
-- get (Reference ld) =
--     let
--         (Document d) =
--             ld ()
--     in
--     Accessor (Path.singleton d.path ()) <| d.data


require : Path -> Accessor r a -> Accessor r a
require path (Accessor paths ra) =
    Accessor (Path.push path paths) ra


requires : Paths -> Accessor r a -> Accessor r a
requires paths_ (Accessor paths ra) =
    Accessor (Path.append paths_ paths) ra
