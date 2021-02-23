module Firestore.Lens exposing (..)

import Array exposing (Array)
import Dict
import Firestore.Access as Access
import Firestore.Internal exposing (..)
import Firestore.Path as Path exposing (Id, Path)
import Firestore.Remote as Remote exposing (Remote(..))
import Firestore.Update as Update
import Html exposing (col)
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Iso as Iso exposing (Iso)


lens : (a -> b) -> (b -> a -> a) -> Lens a b
lens getter setter =
    Lens
        (getter >> UpToDate >> Accessor (Path.rootItem ()))
        (\u b ->
            Updater <|
                \a ->
                    { value = setter b a
                    , updates = Path.rootItem u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )


o : Lens a b -> Lens b c -> Lens a c
o (Lens af uf) (Lens ag ug) =
    Lens
        (\a ->
            let
                (Accessor pas rb) =
                    af a

                (Accessor pbs rc) =
                    Access.unremote <| Remote.map ag rb
            in
            Accessor (Path.joinSub pas pbs) rc
        )
        (\u c ->
            let
                updater _ =
                    Updater <|
                        \a ->
                            let
                                (Accessor paths rb) =
                                    af a
                            in
                            case Remote.toMaybe rb of
                                Nothing ->
                                    { value = a
                                    , updates = Path.empty
                                    , requests = paths
                                    , afterwards = updater ()
                                    }

                                Just b ->
                                    let
                                        ubs =
                                            runUpdater (ug u c) b

                                        uas =
                                            runUpdater (uf u ubs.value) a
                                    in
                                    { value = uas.value
                                    , updates =
                                        Path.joinMap uas.updates <|
                                            \_ -> ubs.updates
                                    , requests = Path.empty
                                    , afterwards = noUpdater
                                    }
            in
            updater ()
        )



-- tuple : Lens a b -> Lens a c -> Lens a (b, c)
-- tuple (Lens af uf) (Lens ag ug) =
-- Iso


fromIso : Iso a b -> Lens a b
fromIso iso =
    lens iso.get (\b _ -> iso.reverseGet b)


composeIso : Iso a b -> Lens b c -> Lens a c
composeIso iso (Lens af uf_) =
    Lens (iso.get >> af >> coerce)
        (\u c ->
            let
                cupd uf =
                    Updater <|
                        \a ->
                            let
                                upd =
                                    runUpdater (uf u c) <| iso.get a
                            in
                            { value = iso.reverseGet upd.value
                            , updates = upd.updates
                            , requests = upd.requests
                            , afterwards = cupd <| \_ _ -> upd.afterwards
                            }
            in
            cupd uf_
        )


isoCompose : Lens a b -> Iso b c -> Lens a c
isoCompose (Lens af uf) iso =
    Lens (af >> Access.map iso.get)
        (\u c -> uf u <| iso.reverseGet c)



-- Basic data types


list : Lens a b -> Lens (List a) (List b)
list (Lens af uf_) =
    Lens
        (List.map af >> Access.list >> coerce)
    <|
        \u bs ->
            let
                listupd uf =
                    Updater <|
                        \xs ->
                            List.foldr
                                (\( a, b ) upds ->
                                    let
                                        upd =
                                            runUpdater (uf u b) a
                                    in
                                    { value = upd.value :: upds.value
                                    , updates =
                                        Path.merge
                                            mergeUpdate
                                            upd.updates
                                            upds.updates
                                    , requests =
                                        Path.append upd.requests upds.requests
                                    , afterwards =
                                        Update.both upds.afterwards <|
                                            listupd uf
                                    }
                                )
                                { value = []
                                , updates = Path.empty
                                , requests = Path.empty
                                , afterwards = noUpdater
                                }
                                (List.zip xs bs)
            in
            listupd uf_


array : Lens a b -> Lens (Array a) (Array b)
array l =
    composeIso (Iso.reverse list2array) <| isoCompose (list l) list2array


atArray : Int -> Lens (Array a) a
atArray i =
    Lens (Array.get i >> Remote.fromMaybe >> Accessor Path.empty)
        (\u a ->
            Updater <|
                \xs ->
                    { value = Array.set i a xs
                    , updates = Path.rootItem u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )



-- Firestore


collection :
    (r -> Collection ss sr)
    -> (Collection ss sr -> r -> r)
    -> Lens r (Collection ss sr)
collection getter setter =
    Lens
        (\r ->
            let
                (Collection col) =
                    getter r
            in
            Accessor
                (Path.singleton (Path.topLevel col.name) ())
                (UpToDate <| Collection col)
        )
        (\u (Collection col) ->
            Updater <|
                \r ->
                    { value = setter (Collection col) r
                    , updates = Path.singleton (Path.topLevel col.name) u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )


subCollection :
    (s -> Collection ss sr)
    -> (Collection ss sr -> s -> s)
    -> Lens (Document s r) (Collection ss sr)
subCollection getter setter =
    Lens
        (\(Document s _) ->
            let
                (Collection col) =
                    getter s
            in
            Accessor
                (Path.singleton (Path.topLevel col.name) ())
                (UpToDate <| Collection col)
        )
        (\u (Collection col) ->
            Updater <|
                \(Document s r) ->
                    { value = Document (setter (Collection col) s) r
                    , updates = Path.singleton (Path.topLevel col.name) u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )


doc : Id -> Lens (Collection s r) (Document s r)
doc id =
    Lens
        (\(Collection col) ->
            Accessor
                (Path.singleton (Path.topLevel id) ())
                (Dict.get id col.docs
                    |> Maybe.withDefault (Document col.empty Loading)
                    |> UpToDate
                )
        )
        (\u d ->
            Updater <|
                \(Collection col) ->
                    { value =
                        Collection
                            { col | docs = Dict.insert id d col.docs }
                    , updates = Path.singleton (Path.topLevel id) u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )


get : Lens (Document s r) r
get =
    Lens (\(Document _ r) -> Accessor (Path.rootItem ()) r)
        (\u r ->
            Updater <|
                \(Document s _) ->
                    { value = Document s (UpToDate r)
                    , updates = Path.rootItem u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )



-- Reference


type Dereferer d a
    = Dereferer (List Id -> ( List Id, Lens d a ))


sub : Lens a (Collection s r) -> Dereferer d a -> Dereferer d (Document s r)
sub sl (Dereferer f) =
    Dereferer <|
        \ids ->
            case f ids of
                ( _ :: id :: ids_, s ) ->
                    ( ids_, o s <| o sl <| doc id )

                ( ids_, s ) ->
                    ( ids_, o s <| fail <| Path.fromList ids_ )


dereferer : Lens d (Collection s r) -> Dereferer d (Document s r)
dereferer l =
    Dereferer <|
        \ids ->
            case ids of
                _ :: id :: ids_ ->
                    ( ids_, o l <| doc id )

                ids_ ->
                    ( ids_, fail <| Path.fromList ids )


deref : Dereferer d (Document s r) -> Reference s r -> Lens d (Document s r)
deref (Dereferer f) (Reference path) =
    case f <| Path.toList path of
        ( [], l ) ->
            l

        ( ids, l ) ->
            fail <| Path.fromList ids


fail : Path -> Lens a b
fail path =
    Lens (\_ -> Accessor (Path.singleton path ()) Failure)
        (\_ _ -> noUpdater)


derefAndAccess :
    Dereferer d (Document s r)
    -> d
    -> Reference s r
    -> Accessor d (Document s r)
derefAndAccess drf d r =
    Access.access (deref drf r) d



-- Iso


list2array : Iso (List a) (Array a)
list2array =
    Iso Array.fromList Array.toList
