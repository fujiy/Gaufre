module Firestore.Internal exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Firestore.Path as Path exposing (Id, Path, PathMap, Paths)
import Firestore.Remote as Remote exposing (Remote(..))
import GDrive exposing (request)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import List


type Collections r
    = Collections r


type Collection s r
    = Collection Id (Dict Id (Document s r))


type Document s r
    = Document s (Remote r)


type Reference d a
    = Reference (Lens d a)


type Lens a b
    = Lens (a -> Accessor a b) (Update -> b -> Updater a)


type Accessor r a
    = Accessor Paths (Remote a)


type Updater a
    = Updater (a -> Updates a)


type alias Updates a =
    { value : a
    , updates : PathMap Update
    , requests : Paths
    , afterwards : Updater a
    }


type Update
    = Set Value
    | Add Value
    | Delete


runUpdater (Updater f) =
    f


type alias Command =
    { listen : Paths
    , unlisten : Paths
    , updates : PathMap Update
    }


type alias Subscription =
    { updates : PathMap Update }


coerce : Accessor r a -> Accessor s a
coerce (Accessor path ra) =
    Accessor path ra



-- type DocumentUpdates
--     = Whole Value
--     | Field String Value
--     | Delete
-- type CollectionUpdates
--     = Add Value


mergeUpdate : Update -> Update -> Update
mergeUpdate u _ =
    u



-- mapDocument : (a -> b) -> Document s a -> Document s b
-- mapDocument f (Document s rd) =
--     Document s <| Remote.map f rd


noUpdates : a -> Updates a
noUpdates a =
    { value = a
    , updates = Path.empty
    , requests = Path.empty
    , afterwards = noUpdater
    }



-- runUpdater : Updater a -> a -> Updates a
-- runUpdater (Updater f) =
--     f


noUpdater : Updater a
noUpdater =
    Updater noUpdates



-- type Refs
--     = Root
--     | Sub (Dict Id Refs)
-- emptyRefs : Refs
-- emptyRefs =
--     Sub Dict.empty
-- subRef : Id -> Refs -> Refs
-- subRef id refs =
--     Sub <| Dict.singleton id refs
-- mergeRefs : List Refs -> Refs
-- mergeRefs =
--     let
--         merge x y =
--             case ( x, y ) of
--                 ( Root, _ ) ->
--                     Root
--                 ( _, Root ) ->
--                     Root
--                 ( Sub dx, Sub dy ) ->
--                     Sub <|
--                         Dict.merge
--                             Dict.insert
--                             (\id a b -> Dict.insert id <| merge a b)
--                             Dict.insert
--                             dx
--                             dy
--                             Dict.empty
--     in
--     List.foldr merge emptyRefs
