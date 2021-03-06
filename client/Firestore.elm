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
    , Timestamp
    , apply
    , compareTimestamp
    , fromPosix
    , getId
    , init
    , ref
    , render
    , serverTimestamp
    , toPosix
    , update
    , watch
    )

import Firestore.Desc as Desc exposing (FirestoreDesc(..))
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path exposing (Path)
import Firestore.Path.Id as Id exposing (Id(..))
import Firestore.Path.Map as PathMap exposing (Paths)
import Firestore.Path.Map.Slice as Slice
import Firestore.Remote as Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Result.Extra exposing (error)
import Time


type Firestore r msg
    = Firestore
        { desc : FirestoreDesc r
        , data : r
        , afterwards : Updater r msg
        , listenings : Paths
        , errors : List Decode.Error
        }


type FirestoreSub r msg
    = FirestoreSub
        { desc : FirestoreDesc r
        , data : r
        , afterwards : Updater r msg
        , listenings : Paths
        , errors : List Decode.Error
        }


type alias Id a =
    Id.Id a


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


type alias Timestamp =
    Internal.Timestamp


ref : Path -> Reference s r
ref =
    Reference


getId : Reference s r -> Id r
getId (Reference p) =
    Path.getLast p |> Maybe.withDefault (Id "")


toPosix : Timestamp -> Maybe Time.Posix
toPosix t =
    case t of
        Timestamp s n ->
            Just <| Time.millisToPosix <| s * 1000 + n // 1000000

        ServerTimestamp ->
            Nothing


fromPosix : Time.Posix -> Timestamp
fromPosix t =
    let
        m =
            Time.posixToMillis t
    in
    Timestamp (m // 1000) (modBy 1000000 m * 1000000)


compareTimestamp : Timestamp -> Timestamp -> Order
compareTimestamp x y =
    case ( x, y ) of
        ( ServerTimestamp, ServerTimestamp ) ->
            EQ

        ( ServerTimestamp, _ ) ->
            GT

        ( _, ServerTimestamp ) ->
            LT

        ( Timestamp sx nx, Timestamp sy ny ) ->
            if sx == sy then
                compare nx ny

            else
                compare sx sy


serverTimestamp : Timestamp
serverTimestamp =
    ServerTimestamp


init : FirestoreDesc r -> Firestore r msg
init (FirestoreDesc d) =
    Firestore
        { desc = FirestoreDesc d
        , data = d.empty
        , afterwards = Update.none
        , listenings = PathMap.empty
        , errors = []
        }


watch :
    SubPort (FirestoreSub r msg)
    -> Firestore r msg
    -> Sub (FirestoreSub r msg)
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
    -> FirestoreSub r msg
    -> ( Firestore r msg, Maybe a, Cmd msg )
apply p use (FirestoreSub fs) =
    update p Update.none use (Firestore fs)


update :
    CmdPort msg
    -> Updater r msg
    -> (r -> Accessor r a)
    -> Firestore r msg
    -> ( Firestore r msg, Maybe a, Cmd msg )
update p updater use (Firestore fs) =
    let
        upds =
            Update.runUpdater (Update.both fs.afterwards updater) fs.data

        (Accessor reqs ra) =
            use upds.value

        requests =
            Slice.toMap mergeRequest getRequest reqs
                |> PathMap.merge mergeRequest upds.requests

        updates =
            PathMap.filterMap toUpdateRequest requests
                |> PathMap.clean

        listenings =
            PathMap.filterMap toGetRequest requests
                |> PathMap.clean

        ( listens, unlistens ) =
            PathMap.diff listenings fs.listenings

        clears =
            PathMap.map (\_ -> Desc.Clear) unlistens

        (FirestoreDesc { applier }) =
            fs.desc

        ( data, errors ) =
            case applier clears upds.value of
                Ok r ->
                    ( r, fs.errors )

                Err err ->
                    ( upds.value, err :: fs.errors )
    in
    ( Firestore
        { fs
            | data = data
            , listenings = listenings
            , afterwards = upds.afterwards
            , errors = errors
        }
    , Remote.toMaybe ra
    , Cmd.batch
        [ p <| encodeCommand <| Command listens unlistens updates
        , upds.command
        ]
    )


render :
    CmdPort msg
    -> (r -> Accessor r a)
    -> Firestore r msg
    -> ( Firestore r msg, Maybe a, Cmd msg )
render p use =
    update p Update.none use


type alias Command =
    { listen : Paths
    , unlisten : Paths
    , updates : PathMap.Map Request
    }


type alias Subscription =
    { updates : PathMap.Map Desc.Update }


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
            (Desc.pathMap Desc.update).decoder
