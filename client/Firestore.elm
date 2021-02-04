module Firestore exposing (..)

import Array
import Firestore.Access as Access
import Firestore.Decode as Decode exposing (Decoder)
import Firestore.Encode as Encode exposing (Encoder)
import Firestore.Internal as Internal exposing (..)
import Firestore.Types as Types exposing (..)
import Firestore.Update as Update
import Json.Decode as Json


type alias Id =
    Types.Id


type alias Firestore r =
    Internal.Firestore r


type alias Collection r =
    Internal.Collection r


type alias Document r =
    Internal.Document r


type alias Reference r =
    Internal.Reference r



-- type alias Remote =
--     Internal.Remote(..)


type alias WatcherPort msg =
    (Json.Value -> msg) -> Sub msg


type alias UpdaterPort msg =
    Json.Value -> Cmd msg


ref : List Id -> Reference r
ref path =
    Internal.Reference <|
        \_ ->
            Internal.Document { path = Array.fromList path, data = Loading }


init : Decoder (Firestore r) -> Firestore r
init (Decode.Decoder dec) =
    case Json.decodeString dec "{}" of
        Err err ->
            Debug.todo <| Json.errorToString err

        Ok fs ->
            fs


render :
    UpdaterPort msg
    -> Encoder (Firestore r)
    -> (r -> Accessor a)
    -> Firestore r
    -> ( Firestore r, Maybe a, Cmd msg )
render p enc =
    update p enc Update.none


update :
    UpdaterPort msg
    -> Encoder (Firestore r)
    -> Updater (Firestore r)
    -> (r -> Accessor a)
    -> Firestore r
    -> ( Firestore r, Maybe a, Cmd msg )
update p enc upd use (Firestore r) =
    let
        upds =
            runUpdater upd <| Firestore r

        (Firestore new) =
            upds.value

        newfs =
            Firestore { new | laters = Update.both upds.laters new.laters }

        (Accessor paths ma) =
            use new.data

        upds_ =
            { upds | requests = Array.append paths upds.requests }
    in
    ( newfs, ma, p <| unValue <| Encode.updates enc upds_ )


digest :
    UpdaterPort msg
    -> Encoder (Firestore r)
    -> (r -> Accessor a)
    -> Firestore r
    -> ( Firestore r, Maybe a, Cmd msg )
digest p enc use (Firestore r) =
    let
        upds =
            runUpdater r.laters <| Firestore r

        (Firestore new) =
            upds.value

        newfs =
            Firestore { new | laters = upds.laters }

        (Accessor paths ma) =
            use new.data

        upds_ =
            { upds | requests = Array.append paths upds.requests }
    in
    ( newfs, ma, p <| unValue <| Encode.updates enc upds_ )


watch :
    WatcherPort (Firestore r)
    -> Decoder (Firestore r)
    -> Firestore r
    -> Sub (Firestore r)
watch p dec (Firestore r) =
    p <|
        \v ->
            let
                (Firestore new) =
                    Decode.decode dec <| Value v
            in
            Firestore { new | laters = r.laters }
