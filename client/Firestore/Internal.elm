module Firestore.Internal exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import Firestore.Path exposing (Path)
import Firestore.Path.Id exposing (Id)
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Map as PathMap
import Firestore.Path.Map.Slice as Slice exposing (Slice)
import Firestore.Remote exposing (Remote(..))
import Json.Encode as Encode exposing (Value)
import List.Extra as List


type Collection s r
    = Collection
        { name : Id r
        , empty : s
        , docs : IdMap.Map r (Document s r)
        , q : Dict QueryKey (Collection s r)
        }


type Document s r
    = Document s (Remote r)


type Reference s r
    = Reference Path


type alias QueryKey =
    String


type alias Lens_ p a q b =
    { access : a -> Accessor p q a b
    , update : Request -> b -> Updater p q a
    }


type Lens p a q b
    = Lens (Lens_ p a q b)


type Accessor p q r a
    = Accessor (Slice p q) (Remote a)


type Updater p q a
    = Updater (a -> Updates p q a)


type alias Updates p q a =
    { value : a
    , requests : Slice p q
    , afterwards : Updater p q a
    }


type alias Root =
    PathMap.Map Request


type alias Col =
    PathMap.Col Request


type alias Doc =
    PathMap.Doc Request


type alias Item =
    Request


type Request
    = None
    | Get
    | Set Value
    | Add Value
    | Delete


queryKey : String -> String -> Value -> QueryKey
queryKey field op value =
    field ++ op ++ Encode.encode 0 value


runUpdater : Updater p q a -> a -> Updates p q a
runUpdater (Updater f) =
    f


coerce : Accessor p q r a -> Accessor p q s a
coerce (Accessor path ra) =
    Accessor path ra


mergeRequest : Request -> Request -> Request
mergeRequest x y =
    case ( x, y ) of
        ( None, _ ) ->
            y

        ( _, None ) ->
            x

        ( Get, _ ) ->
            y

        ( _, Get ) ->
            x

        _ ->
            x


noUpdates : a -> Updates p q a
noUpdates a =
    { value = a
    , requests = Slice.nothing
    , afterwards = noUpdater
    }



-- runUpdater : Updater a -> a -> Updates a
-- runUpdater (Updater f) =
--     f


noUpdater : Updater p q a
noUpdater =
    Updater noUpdates


listUpdater : (b -> Updater p q a) -> List b -> Updater p q (List a)
listUpdater updater bs =
    Updater <|
        \xs ->
            List.foldr
                (\( a, b ) upds ->
                    let
                        upd =
                            runUpdater (updater b) a
                    in
                    { value = upd.value :: upds.value
                    , requests = Slice.both upd.requests upds.requests
                    , afterwards =
                        bothUpdater upds.afterwards <| listUpdater updater bs
                    }
                )
                (noUpdates [])
                (List.zip xs bs)


arrayUpdater : (b -> Updater p q a) -> Array b -> Updater p q (Array a)
arrayUpdater updater bs =
    Updater <|
        \xs ->
            Array.foldl
                (\( a, b ) upds ->
                    let
                        upd =
                            runUpdater (updater b) a
                    in
                    { value = Array.push upd.value upds.value
                    , requests = Slice.both upd.requests upds.requests
                    , afterwards =
                        bothUpdater upds.afterwards <| arrayUpdater updater bs
                    }
                )
                (noUpdates Array.empty)
                (Array.zip xs bs)


bothUpdater : Updater p q a -> Updater p q a -> Updater p q a
bothUpdater (Updater f) (Updater g) =
    Updater <|
        \a ->
            let
                ux =
                    f a

                uy =
                    g ux.value
            in
            { value = uy.value
            , requests = Slice.both ux.requests uy.requests
            , afterwards = bothUpdater ux.afterwards uy.afterwards
            }


allUpdater : List (Updater p q a) -> Updater p q a
allUpdater =
    List.foldr bothUpdater noUpdater
