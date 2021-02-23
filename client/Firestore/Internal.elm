module Firestore.Internal exposing (..)

import Array
import Dict exposing (Dict)
import Firestore.Path as Path exposing (Id, Path, PathMap, Paths)
import Firestore.Remote exposing (Remote(..))
import Json.Encode exposing (Value)


type Collections r
    = Collections r


type Collection s r
    = Collection
        { name : Id
        , empty : s
        , docs : Dict Id (Document s r)
        }


type Document s r
    = Document s (Remote r)


type Reference s r
    = Reference Path


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


runUpdater : Updater a -> a -> Updates a
runUpdater (Updater f) =
    f


coerce : Accessor r a -> Accessor s a
coerce (Accessor path ra) =
    Accessor path ra


mergeUpdate : Update -> Update -> Update
mergeUpdate u _ =
    u


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
