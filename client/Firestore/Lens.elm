module Firestore.Lens exposing (..)

import Array exposing (Array)
import Dict
import Firestore.Access as Access
import Firestore.Internal exposing (..)
import Firestore.Path as Path exposing (Id, PathMap, Paths)
import Firestore.Remote as Remote exposing (Remote(..))
import Firestore.Update as Update
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Iso as Iso exposing (Iso)
import Util exposing (flip)


lens : (a -> b) -> (b -> a -> a) -> Lens a b
lens getter setter =
    Lens
        (getter >> UpToDate >> Accessor Path.empty)
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


doc : Id -> Lens (Collection r) (Document r)
doc id =
    Lens
        (\(Collection name docs) ->
            Accessor
                (Path.singleton (Path.fromList [ name, id ]) ())
                (Dict.get id docs
                    |> Maybe.withDefault (Document Loading)
                    |> UpToDate
                )
        )
        (\u d ->
            Updater <|
                \(Collection name docs) ->
                    { value = Collection name <| Dict.insert id d docs
                    , updates = Path.singleton (Path.fromList [ name, id ]) u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )


get : Lens (Document r) r
get =
    Lens (\(Document r) -> Accessor Path.empty r)
        (\u r ->
            Updater <|
                \_ ->
                    { value = Document (UpToDate r)
                    , updates = Path.rootItem u
                    , requests = Path.empty
                    , afterwards = noUpdater
                    }
        )


deref : Lens (Reference r) r
deref =
    Debug.todo ""


list2array : Iso (List a) (Array a)
list2array =
    Iso Array.fromList Array.toList
