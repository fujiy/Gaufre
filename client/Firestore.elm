module Firestore exposing
    ( CmdPort
    , Collection
    , Document
    , Firestore
    , FirestoreSub
    , Id
    , Lens
    , Reference
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
import Firestore.Path as Path exposing (Path, PathMap, Paths)
import Firestore.Remote as Remote exposing (Remote(..))
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


type alias Lens a b =
    Internal.Lens a b


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
        , afterwards = noUpdater
        , listenings = Path.empty
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
                            ( Path.empty, err :: fs.errors )

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
update p (Updater updater) use (Firestore fs) =
    let
        upds =
            updater fs.data

        (Accessor requests ra) =
            use upds.value

        listenings =
            Path.append requests upds.requests

        ( listens, unlistens ) =
            Path.diff listenings fs.listenings
    in
    ( Firestore
        { fs
            | data = upds.value
            , listenings = listenings
            , afterwards = upds.afterwards
        }
    , Remote.toMaybe ra
    , p <| encodeCommand <| Command listens unlistens upds.updates
    )


render :
    CmdPort msg
    -> (r -> Accessor r a)
    -> Firestore r
    -> ( Firestore r, Maybe a, Cmd msg )
render p use =
    update p noUpdater use


type alias Command =
    { listen : Paths
    , unlisten : Paths
    , updates : PathMap Update
    }


type alias Subscription =
    { updates : PathMap Value }


encodeCommand : Command -> Value
encodeCommand command =
    Encode.object
        [ ( "listen", Desc.paths.encoder command.listen )
        , ( "unlisten", Desc.paths.encoder command.unlisten )
        , ( "updates", (Desc.pathMap updateDesc).encoder command.updates )
        ]


decodeSubscription : Decode.Decoder Subscription
decodeSubscription =
    Decode.map Subscription <|
        Decode.field "updates" <|
            (Desc.pathMap Desc.value).decoder


updateDesc : Desc Update
updateDesc =
    Desc
        (\u ->
            case u of
                Set v ->
                    Encode.object
                        [ ( "type", Encode.string "set" ), ( "value", v ) ]

                Add v ->
                    Encode.object
                        [ ( "type", Encode.string "add" ), ( "value", v ) ]

                Delete ->
                    Encode.object [ ( "type", Encode.string "delete" ) ]
        )
        (Decode.andThen
            (\t ->
                case t of
                    "set" ->
                        Decode.map Set <|
                            Decode.field "value" Decode.value

                    "add" ->
                        Decode.map Add <|
                            Decode.field "value" Decode.value

                    "delete" ->
                        Decode.succeed Delete

                    _ ->
                        Decode.fail <| "unknown type: " ++ t
            )
            (Decode.field "type" Decode.string)
        )
