module Firestore exposing (..)

import Firestore.Decode as Decode exposing (Decoder)
import Firestore.Encode as Encode exposing (Encoder)
import Firestore.Internal as Internal exposing (..)
import Firestore.Update as Update
import Json.Decode as Json


type alias Id =
    String


type alias Firestore r =
    Internal.Firestore r


type alias Collection r =
    Internal.Collection r


type alias Document r =
    Internal.Document r


type alias Reference r =
    Internal.Reference r


type alias WatcherPort msg =
    (Json.Value -> msg) -> Sub msg


type alias UpdaterPort msg =
    Json.Value -> Cmd msg


init : Decoder (Firestore r) -> Firestore r
init (Decode.Decoder dec) =
    case Json.decodeString dec "{}" of
        Err err ->
            Debug.todo <| Json.errorToString err

        Ok fs ->
            fs


update :
    UpdaterPort msg
    -> Encoder (Firestore r)
    -> Updater (Firestore r)
    -> Firestore r
    -> ( Firestore r, Cmd msg )
update p enc upd (Firestore r) =
    let
        upds =
            runUpdater upd <| Firestore r

        (Firestore new) =
            upds.value

        newfs =
            Firestore { new | laters = Update.both upds.laters new.laters }
    in
    ( newfs, p <| unValue <| Encode.updates enc upds )


digest :
    UpdaterPort msg
    -> Encoder (Firestore r)
    -> Firestore r
    -> ( Firestore r, Cmd msg )
digest p enc (Firestore r) =
    let
        upds =
            runUpdater r.laters <| Firestore r

        (Firestore new) =
            upds.value

        newfs =
            Firestore { new | laters = upds.laters }
    in
    ( newfs, p <| unValue <| Encode.updates enc upds )


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
