module Firestore.Lens exposing (..)

import Array exposing (Array, slice)
import Dict
import Firestore.Access as Access
import Firestore.Desc as Desc exposing (Desc)
import Firestore.Internal exposing (..)
import Firestore.Path as Path exposing (Path)
import Firestore.Path.Id as Id exposing (Id(..))
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Map as PathMap
import Firestore.Path.Map.Slice as Slice
import Firestore.Remote as Remote exposing (Remote(..))
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Iso as Iso exposing (Iso)


lens : (a -> b) -> (b -> a -> a) -> Lens x a x b
lens getter setter =
    Lens
        { access = getter >> Access.just
        , update =
            \_ b ->
                Updater <|
                    \a ->
                        { value = setter b a
                        , requests = Slice.zero
                        , afterwards = noUpdater
                        }
        }


option : (a -> Maybe b) -> (b -> a -> a) -> Lens x a x b
option getter setter =
    Lens
        { access = getter >> Remote.fromMaybe >> Accessor Slice.zero
        , update =
            \_ b ->
                Updater <|
                    \a ->
                        { value = setter b a
                        , requests = Slice.zero
                        , afterwards = noUpdater
                        }
        }


ident : Lens x a x a
ident =
    Lens
        { access = \a -> Access.just a
        , update =
            \_ a ->
                Updater <|
                    \_ ->
                        { value = a
                        , requests = Slice.zero
                        , afterwards = noUpdater
                        }
        }


o : Lens o a p b -> Lens p b q c -> Lens o a q c
o (Lens ll) (Lens lr) =
    Lens
        { access =
            \a ->
                let
                    (Accessor sop rb) =
                        ll.access a

                    (Accessor spq rc) =
                        Access.unremote <| Remote.map lr.access rb
                in
                -- case rb of
                --     Failure ->
                --         Accessor Slice.nothing Failure
                --     Loading ->
                --         Accessor Slice.nothing Loading
                --     Committing b ->
                --         let
                --             (Accessor spq rc) =
                --                 lr.access b
                --         in
                --         Accessor (Slice.compose sop spq) rc
                --     UpToDate b ->
                --         let
                --             (Accessor spq rc) =
                --                 lr.access b
                --         in
                Accessor (Slice.compose sop spq) rc
        , update =
            \u c ->
                let
                    updater _ =
                        Updater <|
                            \a ->
                                let
                                    (Accessor sop rb) =
                                        ll.access a
                                in
                                case Remote.toMaybe rb of
                                    Nothing ->
                                        { value = a
                                        , requests = Slice.nothing
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
                                                runUpdater
                                                    (lr.update u c)
                                                    b

                                            uas =
                                                runUpdater
                                                    (ll.update noRequest
                                                        ubs.value
                                                    )
                                                    a
                                        in
                                        { value = uas.value
                                        , requests =
                                            Slice.compose
                                                (Slice.both sop uas.requests)
                                                ubs.requests
                                        , afterwards = noUpdater
                                        }
                in
                updater ()
        }


const : a -> Lens p b q a
const a =
    Lens
        { access = \_ -> Accessor Slice.nothing <| UpToDate a
        , update = \_ _ -> noUpdater
        }



-- tuple : Lens a b -> Lens a c -> Lens a (b, c)
-- tuple (Lens af uf) (Lens ag ug) =
-- Iso


fromIso : Iso a b -> Lens x a x b
fromIso i =
    lens i.get (\b _ -> i.reverseGet b)


iso : (a -> b) -> (b -> a) -> Lens x a x b
iso getter rev =
    lens getter (\b _ -> rev b)



-- -- Basic data types


list : Lens p a q b -> Lens p (List a) q (List b)
list (Lens l) =
    Lens
        { access = List.map l.access >> Access.list >> coerce
        , update = \u -> listUpdater (l.update u)
        }


array : Lens p a q b -> Lens p (Array a) q (Array b)
array (Lens l) =
    Lens
        { access = Array.map l.access >> Access.array >> coerce
        , update = \u -> arrayUpdater (l.update u)
        }


atArray : Int -> Lens x (Array a) x a
atArray i =
    option (Array.get i) (Array.set i)



-- Firestore


collection :
    (r -> Collection ss sr)
    -> (Collection ss sr -> r -> r)
    -> Lens Root r Col (Collection ss sr)
collection getter setter =
    Lens
        { access =
            \r ->
                let
                    (Collection col) =
                        getter r
                in
                Accessor (Slice.col col.name) (UpToDate <| Collection col)
        , update =
            \u (Collection col) ->
                Updater <|
                    \r ->
                        { value = setter (Collection col) r
                        , requests =
                            Slice.addCol (Slice.col col.name)
                                (PathMap.colRootItem u)
                        , afterwards = noUpdater
                        }
        }


subCollection :
    (s -> Collection ss sr)
    -> (Collection ss sr -> s -> s)
    -> Lens Doc (Document s r) Col (Collection ss sr)
subCollection getter setter =
    Lens
        { access =
            \(Document s _) ->
                let
                    (Collection col) =
                        getter s
                in
                Accessor (Slice.subCol col.name) (UpToDate <| Collection col)
        , update =
            \u (Collection col) ->
                Updater <|
                    \(Document s r) ->
                        { value = Document (setter (Collection col) s) r
                        , requests =
                            Slice.addSubCol (Slice.subCol col.name)
                                (PathMap.colRootItem u)
                        , afterwards = noUpdater
                        }
        }


doc : Id r -> Lens Col (Collection s r) Doc (Document s r)
doc id =
    Lens
        { access =
            \(Collection col) ->
                Accessor
                    (Slice.doc id)
                    (IdMap.get id col.docs
                        |> Maybe.withDefault (Document col.empty Loading)
                        |> UpToDate
                    )
        , update =
            \u d ->
                Updater <|
                    \(Collection col) ->
                        { value =
                            Collection
                                { col | docs = IdMap.insert id d col.docs }
                        , requests =
                            Slice.addDoc (Slice.doc id) (PathMap.docRootItem u)
                        , afterwards = noUpdater
                        }
        }


type QueryOp
    = EQ
    | NE
    | GT
    | GE
    | LT
    | LE
    | IN
    | NOT_IN
    | CONTAINS
    | CONTAINS_ANY


opToString : QueryOp -> String
opToString op =
    case op of
        EQ ->
            "=="

        NE ->
            "!="

        GT ->
            ">"

        GE ->
            ">="

        LT ->
            "<"

        LE ->
            "<="

        IN ->
            "in"

        NOT_IN ->
            "not-in"

        CONTAINS ->
            "array-contains"

        CONTAINS_ANY ->
            "array-contains-any"


where_ :
    String
    -> QueryOp
    -> Desc a
    -> a
    -> Lens Col (Collection s r) Col (Collection s r)
where_ field qop desc a =
    let
        op =
            opToString qop

        value =
            desc.encoder a

        key =
            queryKey field op value
    in
    Lens
        { access =
            \(Collection col) ->
                Accessor
                    (Slice.query field op value)
                    (let
                        (Collection qcol) =
                            Dict.get key col.q
                                |> Maybe.withDefault
                                    (Collection
                                        { col
                                            | docs = IdMap.empty
                                            , q = Dict.empty
                                            , all = Loading
                                        }
                                    )
                     in
                     -- if col.loading then
                     --    Loading
                     -- else
                     UpToDate <| Collection qcol
                    )
        , update =
            \u qc ->
                Updater <|
                    \(Collection col) ->
                        { value =
                            Collection
                                { col | q = Dict.insert key qc col.q }
                        , requests =
                            Slice.addCol
                                (Slice.query field op value)
                                (PathMap.colRootItem u)
                        , afterwards = noUpdater
                        }
        }


whereEmpty : String -> Lens Col (Collection s r) Col (Collection s r)
whereEmpty field =
    where_ field EQ (Desc.list Desc.string) []


whereNotEmpty : String -> Lens Col (Collection s r) Col (Collection s r)
whereNotEmpty field =
    where_ field NE (Desc.list Desc.string) []


getAll : Lens Col (Collection s r) Item (List r)
getAll =
    Lens
        { access =
            \(Collection col) ->
                Accessor
                    Slice.colItem
                    (Remote.map
                        (List.map (\(Document _ r) -> r) >> Remote.cats)
                        col.all
                    )
        , update =
            \_ _ ->
                Updater <|
                    \col ->
                        { value = col
                        , requests = Slice.colItem
                        , afterwards = noUpdater
                        }
        }


get : Lens Doc (Document s r) Item r
get =
    Lens
        { access = \(Document _ r) -> Accessor Slice.docItem r
        , update =
            \_ r ->
                Updater <|
                    \(Document s _) ->
                        { value = Document s <| UpToDate r
                        , requests = Slice.docItem
                        , afterwards = noUpdater
                        }
        }


gets : Lens Doc (List (Document s r)) Item (List r)
gets =
    list get


getRemote : Lens Doc (Document s r) Item (Remote r)
getRemote =
    Lens
        { access = \(Document _ r) -> Accessor Slice.docItem (UpToDate r)
        , update =
            \_ rr ->
                Updater <|
                    \(Document s _) ->
                        { value = Document s rr
                        , requests = Slice.docItem
                        , afterwards = noUpdater
                        }
        }



-- Reference
-- ref : Lens Root a Doc (Document s r) -> Lens Root a Item (Reference s r)
-- ref (Lens l) =
--     Lens
--         { access =
--             \a ->
--                 let
--                     (Accessor s _) =
--                         l.access a
--                 in
--                 Slice.toMapDoc mergeRequest getRequest s
--                     |> PathMap.toList
--                     |> List.head
--                     |> Maybe.unwrap Path.root Tuple.first
--                     |> Reference
--         , update =
--             \_ _ ->
--                 Updater <|
--                     \a ->
--                         { value = a
--                         , requests = Slice.just
--                         , afterwards = noUpdater
--                         }
--         }


type Dereferer d a
    = Dereferer (Path.Doc -> Lens Doc d Doc a)


type RootDereferer d a
    = RootDereferer (Path -> Lens Root d Doc a)


sub :
    Lens Doc d Col (Collection rs rr)
    -> Dereferer (Document rs rr) (Document s r)
    -> Dereferer d (Document s r)
sub superLens (Dereferer f) =
    Dereferer <|
        \docPath ->
            case docPath of
                Path.SubCol _ (Path.SubDoc id subPath) ->
                    o superLens <| o (doc <| Id id) <| f subPath

                _ ->
                    failDoc docPath


root :
    Lens Root d Col (Collection rs rr)
    -> Dereferer (Document rs rr) (Document s r)
    -> RootDereferer d (Document s r)
root superLens (Dereferer f) =
    RootDereferer <|
        \path ->
            case path of
                Path.RootCol _ (Path.SubDoc id subPath) ->
                    o superLens <| o (doc <| Id id) <| f subPath

                _ ->
                    fail


end : Dereferer a a
end =
    Dereferer <|
        \docPath ->
            case docPath of
                Path.Doc ->
                    Lens
                        { access = \a -> Access.just a
                        , update =
                            \u a ->
                                Updater <|
                                    \_ ->
                                        { value = a
                                        , requests =
                                            Slice.addDoc Slice.zero
                                                (PathMap.docRootItem u)
                                        , afterwards = noUpdater
                                        }
                        }

                _ ->
                    fail


failDoc : Path.Doc -> Lens Doc a Doc b
failDoc path =
    Lens
        { access = \_ -> Accessor (Slice.singletonDocDoc path) Failure
        , update = \_ _ -> noUpdater
        }


fail : Lens p a q b
fail =
    Lens
        { access = \_ -> Access.failure
        , update = \_ _ -> noUpdater
        }


derefs :
    Lens Root d Col (Collection rs rr)
    -> Dereferer (Document rs rr) (Document s r)
    -> Lens Root d Item (List (Reference s r))
    -> Lens Root d Doc (List (Document s r))
derefs rl drf (Lens l) =
    let
        rdrf =
            root rl drf
    in
    Lens
        { access =
            \d ->
                let
                    (Accessor rs rrs) =
                        l.access d
                in
                Remote.traverse List.map (always []) rrs
                    |> List.map
                        (\rr -> derefAccessor rdrf (Accessor rs rr) d)
                    |> Access.list
        , update =
            \u docs ->
                List.indexedMap Tuple.pair docs
                    |> List.map
                        (\( i, dc ) ->
                            derefUpdater rdrf
                                (l.access
                                    >> Access.map (List.getAt i)
                                    >> Access.fromJust
                                )
                                u
                                dc
                        )
                    |> allUpdater
        }


deref :
    Lens Root d Col (Collection rs rr)
    -> Dereferer (Document rs rr) (Document s r)
    -> Lens Root d Item (Reference s r)
    -> Lens Root d Doc (Document s r)
deref rl drf (Lens l) =
    let
        rdrf =
            root rl drf
    in
    Lens
        { access = \d -> derefAccessor rdrf (l.access d) d
        , update = derefUpdater rdrf l.access
        }


derefAccessor :
    RootDereferer d (Document s r)
    -> Accessor Root Item d (Reference s r)
    -> d
    -> Accessor Root Doc d (Document s r)
derefAccessor (RootDereferer f) (Accessor rs rr) d =
    let
        (Accessor ds rdoc) =
            Remote.map
                (\(Reference path) ->
                    let
                        (Lens l) =
                            f path
                    in
                    l.access d
                )
                rr
                |> Access.unremote
    in
    Accessor (Slice.also rs getRequest ds) rdoc


derefUpdater :
    RootDereferer d (Document s r)
    -> (d -> Accessor Root Item d (Reference s r))
    -> Request
    -> Document s r
    -> Updater Root Doc d
derefUpdater (RootDereferer f) acc u dc =
    Updater <|
        \d ->
            let
                (Accessor rs rr) =
                    acc d
            in
            case Remote.toMaybe rr of
                Nothing ->
                    { value = d
                    , requests = Slice.also rs getRequest Slice.nothing
                    , afterwards =
                        case rr of
                            Loading ->
                                derefUpdater (RootDereferer f) acc u dc

                            _ ->
                                noUpdater
                    }

                Just (Reference path) ->
                    let
                        (Lens l) =
                            f path

                        updates upds =
                            { value = upds.value
                            , requests = Slice.also rs getRequest upds.requests
                            , afterwards =
                                Updater <| runUpdater upds.afterwards >> updates
                            }
                    in
                    updates <| runUpdater (l.update u dc) d



-- Iso


reverse =
    Iso.reverse


list2array : Iso (List a) (Array a)
list2array =
    Iso Array.fromList Array.toList
