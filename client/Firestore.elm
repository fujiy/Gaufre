module Firestore exposing
    ( CmdPort
    , Col
    , Collection
    , Doc
    , Document
    , Firestore
    , FirestoreSub
    , Id
    , Item
    , Lens
    , Reference
    , Root
    , SubPort
    , apply
    , getId
    , init
    , ref
    , render
    , update
    , watch
    )

import Firestore.Desc as Desc exposing (Desc, FirestoreDesc(..))
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path exposing (Path)
import Firestore.Path.Map as PathMap exposing (Paths)
import Firestore.Path.Map.Slice as Slice
import Firestore.Remote as Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


type Firestore r
    = Firestore
        { desc : FirestoreDesc r
        , data : r
        , afterwards : Updater r
        , listenings : Paths
        , errors : List Decode.Error
        }


type FirestoreSub r
    = FirestoreSub
        { desc : FirestoreDesc r
        , data : r
        , afterwards : Updater r
        , listenings : Paths
        , errors : List Decode.Error
        }


type alias Id =
    Path.Id


type alias Collection s r =
    Internal.Collection s r


type alias Document s r =
    Internal.Document s r


type alias Reference s r =
    Internal.Reference s r


type alias Lens p a q b =
    Internal.Lens p a q b


type alias Root =
    Internal.Root


type alias Col =
    Internal.Col


type alias Doc =
    Internal.Doc


type alias Item =
    Internal.Item


type alias Accessor r a =
    Internal.Accessor Root Item r a


type alias SubPort msg =
    (Value -> msg) -> Sub msg


type alias CmdPort msg =
    Value -> Cmd msg


ref : Path -> Reference s r
ref =
    Reference


getId : Reference s r -> Id
getId (Reference p) =
    Path.getLast p |> Maybe.withDefault ""


init : FirestoreDesc r -> Firestore r
init (FirestoreDesc d) =
    Firestore
        { desc = FirestoreDesc d
        , data = d.empty
        , afterwards = Update.none
        , listenings = PathMap.empty
        , errors = []
        }


watch : SubPort (FirestoreSub r) -> Firestore r -> Sub (FirestoreSub r)
watch p (Firestore fs) =
    p <|
        \v ->
            let
                ( updates, errors ) =
                    case Decode.decodeValue decodeSubscription v of
                        Ok sub ->
                            ( sub.updates, fs.errors )

                        Err err ->
                            ( PathMap.empty, err :: fs.errors )

                (FirestoreDesc { applier }) =
                    fs.desc
            in
            case applier updates fs.data of
                Ok r ->
                    FirestoreSub { fs | data = r, errors = errors }

                Err err ->
                    FirestoreSub { fs | errors = err :: errors }


apply :
    CmdPort msg
    -> (r -> Accessor r a)
    -> FirestoreSub r
    -> ( Firestore r, Maybe a, Cmd msg )
apply p use (FirestoreSub fs) =
    update p fs.afterwards use (Firestore fs)


update :
    CmdPort msg
    -> Updater r
    -> (r -> Accessor r a)
    -> Firestore r
    -> ( Firestore r, Maybe a, Cmd msg )
update p updater use (Firestore fs) =
    let
        upds =
            Update.runUpdater updater fs.data

        (Accessor reqs ra) =
            use upds.value

        requests =
            Slice.toMap mergeRequest Get reqs
                |> PathMap.merge mergeRequest upds.requests

        updates =
            PathMap.filterMap isUpdateRequest requests
                |> PathMap.clean

        listenings =
            PathMap.filterMap isGetRequest requests
                |> PathMap.clean

        ( listens, unlistens ) =
            PathMap.diff listenings fs.listenings
    in
    ( Firestore
        { fs
            | data = upds.value
            , listenings = listenings
            , afterwards = upds.afterwards
        }
    , Remote.toMaybe ra
    , p <| encodeCommand <| Command listens unlistens updates
    )


render :
    CmdPort msg
    -> (r -> Accessor r a)
    -> Firestore r
    -> ( Firestore r, Maybe a, Cmd msg )
render p use =
    update p Update.none use


isGetRequest : Request -> Maybe ()
isGetRequest req =
    case req of
        None ->
            Nothing

        _ ->
            Just ()


isUpdateRequest : Request -> Maybe Request
isUpdateRequest req =
    case req of
        None ->
            Nothing

        Get ->
            Nothing

        _ ->
            Just req


type alias Command =
    { listen : Paths
    , unlisten : Paths
    , updates : PathMap.Map Request
    }


type alias Subscription =
    { updates : PathMap.Map Value }


encodeCommand : Command -> Value
encodeCommand command =
    Encode.object
        [ ( "listen", Desc.paths.encoder command.listen )
        , ( "unlisten", Desc.paths.encoder command.unlisten )
        , ( "updates", (Desc.pathMap Desc.request).encoder command.updates )
        ]


decodeSubscription : Decode.Decoder Subscription
decodeSubscription =
    Decode.map Subscription <|
        Decode.field "updates" <|
            (Desc.pathMap Desc.value).decoder
