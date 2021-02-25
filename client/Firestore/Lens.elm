module Firestore.Lens exposing (..)

import Array exposing (Array)
import Browser.Navigation exposing (load)
import Dict
import Firestore.Access as Access
import Firestore.Desc exposing (documentWithSubs, paths)
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
                                    , afterwards =
                                        case rb of
                                            Loading ->
                                                updater ()

                                            _ ->
                                                noUpdater
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


isoCompose : Iso a b -> Lens b c -> Lens a c
isoCompose iso (Lens af uf_) =
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


composeIso : Lens a b -> Iso b c -> Lens a c
composeIso (Lens af uf) iso =
    Lens (af >> Access.map iso.get)
        (\u c -> uf u <| iso.reverseGet c)



-- Basic data types


list : Lens a b -> Lens (List a) (List b)
list (Lens af uf) =
    Lens
        (List.map af >> Access.list >> coerce)
        (\u -> Update.list (uf u))


array : Lens a b -> Lens (Array a) (Array b)
array l =
    isoCompose (reverse list2array) <| composeIso (list l) list2array


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


getAll : Lens (Collection s r) (List ( Id, Remote r ))
getAll =
    Lens
        (\(Collection col) ->
            Accessor
                (Path.rootItem ())
                (Dict.toList col.docs
                    |> List.map (\( id, Document _ r ) -> ( id, r ))
                    |> UpToDate
                )
        )
        (\u xs ->
            Updater <|
                \(Collection col) ->
                    { value =
                        Collection
                            { col
                                | docs =
                                    List.foldr
                                        (\( id, r ) ->
                                            Dict.insert id <|
                                                Document col.empty r
                                        )
                                        col.docs
                                        xs
                            }
                    , updates = Path.rootItem u
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


getRemote : Lens (Document s r) (Remote r)
getRemote =
    Lens (\(Document _ r) -> Accessor (Path.rootItem ()) (UpToDate r))
        (\u rr ->
            Updater <|
                \(Document s _) ->
                    { value = Document s rr
                    , updates = Path.rootItem u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )



-- Reference


ref : Lens a (Document s r) -> a -> Reference s r
ref (Lens acc _) a =
    let
        (Accessor paths _) =
            acc a
    in
    Path.toList paths
        |> List.head
        |> Maybe.unwrap Path.root Tuple.first
        |> Reference


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
                    ( ids_, o s <| fail <| Path.fromIds ids_ )


dereferer : Lens d (Collection s r) -> Dereferer d (Document s r)
dereferer l =
    Dereferer <|
        \ids ->
            case ids of
                _ :: id :: ids_ ->
                    ( ids_, o l <| doc id )

                ids_ ->
                    ( ids_, fail <| Path.fromIds ids )


derefs :
    Dereferer d (Document s r)
    -> Lens d (List (Reference s r))
    -> Lens d (List (Document s r))
derefs drf (Lens acc _) =
    Lens
        (\d ->
            let
                (Accessor paths rrs) =
                    acc d
            in
            Remote.traverse List.map (always []) rrs
                |> List.map
                    (\rr -> derefAccessor drf (Accessor paths rr) d)
                |> Access.list
        )
        (\u docs ->
            List.indexedMap Tuple.pair docs
                |> List.map
                    (\( i, dc ) ->
                        derefUpdater drf
                            (acc
                                >> Access.map (List.getAt i)
                                >> Access.fromJust
                            )
                            u
                            dc
                    )
                |> Update.all
        )


derefAccessor :
    Dereferer d (Document s r)
    -> Accessor d (Reference s r)
    -> d
    -> Accessor d (Document s r)
derefAccessor (Dereferer f) (Accessor paths rr) d =
    let
        (Accessor dpaths rdoc) =
            Remote.map
                (\(Reference path) ->
                    case f <| Path.toIds path of
                        ( [], Lens acc_ _ ) ->
                            acc_ d

                        _ ->
                            Access.failure
                )
                rr
                |> Access.unremote
    in
    Accessor (Path.append paths dpaths) rdoc


derefUpdater :
    Dereferer d (Document s r)
    -> (d -> Accessor d (Reference s r))
    -> Update
    -> Document s r
    -> Updater d
derefUpdater (Dereferer f) acc u dc =
    Updater <|
        \d ->
            let
                (Accessor paths rr) =
                    acc d
            in
            case Remote.toMaybe rr of
                Nothing ->
                    { value = d
                    , updates = Path.empty
                    , requests = paths
                    , afterwards =
                        case rr of
                            Loading ->
                                derefUpdater (Dereferer f) acc u dc

                            _ ->
                                noUpdater
                    }

                Just (Reference path) ->
                    case f <| Path.toIds path of
                        ( [], Lens _ upd_ ) ->
                            let
                                upds =
                                    runUpdater (upd_ u dc) d
                            in
                            { upds
                                | requests =
                                    Path.append paths
                                        upds.requests
                            }

                        _ ->
                            noUpdates d


deref :
    Dereferer d (Document s r)
    -> Lens d (Reference s r)
    -> Lens d (Document s r)
deref drf (Lens acc _) =
    Lens
        (\d -> derefAccessor drf (acc d) d)
        (derefUpdater drf acc)


fail : Path -> Lens a b
fail path =
    Lens (\_ -> Accessor (Path.singleton path ()) Failure)
        (\_ _ -> noUpdater)



-- Iso


reverse =
    Iso.reverse


list2array : Iso (List a) (Array a)
list2array =
    Iso Array.fromList Array.toList
