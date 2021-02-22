module Firestore exposing (..)

-- import Firestore.Update as Update

import Array
import Dict
import Firestore.Access as Access
import Firestore.Desc exposing (FirestoreDesc)
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path exposing (Id, Path, Paths)
import Firestore.Remote exposing (Remote(..))
import Json.Encode exposing (Value)


type Firestore r
    = Firestore
        { data : r
        , laters : Updater (Firestore r)
        , listenings : Paths
        , desc : FirestoreDesc r
        }


type alias Collection r =
    Internal.Collection r


type alias Document r =
    Internal.Document r


type alias Reference r =
    Internal.Reference r


type alias Lens a b =
    Internal.Lens a b



-- type alias Remote =
--     Internal.Remote(..)


type alias WatcherPort msg =
    (Value -> msg) -> Sub msg


type alias UpdaterPort msg =
    Value -> Cmd msg


ref : Path -> Reference r
ref p =
    Reference p (\_ -> Document Loading)


collection : Collection r
collection =
    Internal.Collection "" Dict.empty



-- init : Decoder (Firestore r) -> Firestore r
-- init (Decode.Decoder dec) =
--     case Json.decodeString dec "{}" of
--         Err err ->
--             Debug.todo <| Json.errorToString err
--         Ok fs ->
--             fs
-- render :
--     UpdaterPort msg
--     -> Encoder (Firestore r)
--     -> (r -> Accessor a)
--     -> Firestore r
--     -> ( Firestore r, Maybe a, Cmd msg )
-- render p enc =
--     update p enc Update.none
-- update :
--     UpdaterPort msg
--     -> Encoder (Firestore r)
--     -> Updater (Firestore r)
--     -> (r -> Accessor a)
--     -> Firestore r
--     -> ( Firestore r, Maybe a, Cmd msg )
-- update p enc upd use (Firestore r) =
--     let
--         upds =
--             runUpdater upd <| Firestore r
--         (Firestore new) =
--             upds.value
--         newfs =
--             Firestore { new | laters = Update.both upds.laters new.laters }
--         (Accessor paths ma) =
--             use new.data
--         upds_ =
--             { upds | requests = Array.append paths upds.requests }
--     in
--     ( newfs, ma, p <| unValue <| Encode.updates enc upds_ )
-- digest :
--     UpdaterPort msg
--     -> Encoder (Firestore r)
--     -> (r -> Accessor a)
--     -> Firestore r
--     -> ( Firestore r, Maybe a, Cmd msg )
-- digest p enc use (Firestore r) =
--     let
--         upds =
--             runUpdater r.laters <| Firestore r
--         (Firestore new) =
--             upds.value
--         newfs =
--             Firestore { new | laters = upds.laters }
--         (Accessor paths ma) =
--             use new.data
--         upds_ =
--             { upds | requests = Array.append paths upds.requests }
--     in
--     ( newfs, ma, p <| unValue <| Encode.updates enc upds_ )
-- watch :
--     WatcherPort (Firestore r)
--     -> Decoder (Firestore r)
--     -> Firestore r
--     -> Sub (Firestore r)
-- watch p dec (Firestore r) =
--     p <|
--         \v ->
--             let
--                 (Firestore new) =
--                     Decode.decode dec <| Value v
--             in
--             Firestore { new | laters = r.laters }
