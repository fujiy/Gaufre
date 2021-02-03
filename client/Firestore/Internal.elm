module Firestore.Internal exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Firestore.Types exposing (..)
import Json.Encode as JE
import List


type Firestore r
    = Firestore
        { data : r
        , laters : Updater (Firestore r)
        }


type Collection r
    = Collection
        { path : Path
        , documents : Dict Id (Document r)
        }


type Document r
    = Document
        { path : Path
        , data : Remote r
        }


type Reference r
    = Reference (() -> Document r)


type Value a
    = Value JE.Value


type Updater a
    = Updater (a -> Updates a)


type Accessor a
    = Accessor (Array Path) (Maybe a)


type alias Updates a =
    { value : a
    , documents : Array ( Path, DocumentUpdates )
    , collections : Array ( Path, CollectionUpdates )
    , laters : Updater a
    , requests : Array Path
    }


type DocumentUpdates
    = Whole (Value ())
    | Field String (Value ())
    | Delete


type CollectionUpdates
    = Add (Value ())


mapDocument : (a -> b) -> Document a -> Document b
mapDocument f (Document { path, data }) =
    Document { path = path, data = mapRemote f data }


noUpdates : a -> Updates a
noUpdates a =
    Updates a Array.empty Array.empty noUpdater Array.empty


runUpdater : Updater a -> a -> Updates a
runUpdater (Updater f) =
    f


noUpdater : Updater a
noUpdater =
    Updater noUpdates


fromJson : JE.Value -> Value a
fromJson =
    Value


unValue : Value a -> JE.Value
unValue (Value v) =
    v


loadingDocument : Path -> Document r
loadingDocument path =
    Document { path = path, data = Loading }


noDocument : Path -> Document r
noDocument path =
    Document { path = path, data = Failure }



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
