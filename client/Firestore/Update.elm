module Firestore.Update exposing (..)

import Array exposing (Array)
import Dict
import Firestore.Decode as Decode exposing (Decoder)
import Firestore.Encode as Encode exposing (Encoder)
import Firestore.Internal as Internal exposing (..)
import Json.Decode
import Maybe.Extra as Maybe
import Monocle.Lens exposing (Lens)
import Util exposing (..)


type alias Updater a =
    Internal.Updater a


updates : List (Updater a) -> Updater a
updates upds =
    List.foldr both noUpdater upds


none : Updater a
none =
    noUpdater


firestore : Updater r -> Updater (Firestore r)
firestore upd =
    Updater <|
        \(Firestore fs) ->
            runUpdater upd fs.data
                |> mapUpdates (\r -> Firestore { fs | data = r }) firestore


collection :
    Lens r (Collection a)
    -> Updater (Collection a)
    -> Updater r
collection lens upd =
    Updater <|
        \r ->
            lens.get r
                |> runUpdater upd
                |> mapUpdates
                    (flip lens.set r)
                    (collection lens)


doc : Id -> Updater (Document r) -> Updater (Collection r)
doc id (Updater f) =
    Updater <|
        \(Collection c) ->
            Dict.get id c.documents
                |> Maybe.withDefault (loadingDocument (sub c.path id))
                |> f
                |> mapUpdates
                    (\d ->
                        Collection <|
                            { c | documents = Dict.insert id d c.documents }
                    )
                    (doc id)


set : Encoder (Document r) -> Document r -> Updater (Document r)
set enc d =
    Updater <|
        \(Document old) ->
            let
                upd =
                    noUpdates <| Document old
            in
            { upd
                | documents =
                    Array.fromList
                        [ ( old.path, Whole <| Encode.coerce <| enc d ) ]
            }


add : Encoder (Document r) -> Document r -> Updater (Collection r)
add enc d =
    Updater <|
        \(Collection old) ->
            let
                upd =
                    noUpdates <| Collection old
            in
            { upd
                | collections =
                    Array.fromList
                        [ ( old.path, Add <| Encode.coerce <| enc d ) ]
            }



-- field : String -> Value () -> Updater (Document r)
-- field name v =
--     Updater <|
--         \(Document d) ->
--             { noUpdates
--                 | documents =
--                     Array.fromList
--                         [ ( d.path, Field name v ) ]
--             }


modify : Encoder (Document r) -> (r -> r) -> Updater (Document r)
modify enc f =
    alter enc <| Maybe.map f


alter : Encoder (Document r) -> (Maybe r -> Maybe r) -> Updater (Document r)
alter enc f =
    Updater <|
        \(Document old) ->
            let
                whole d =
                    Whole <|
                        Encode.coerce <|
                            enc <|
                                Document { old | data = d }

                upd =
                    noUpdates <| Document old

                mod a =
                    case f <| Just a of
                        Nothing ->
                            { upd
                                | value = Document { old | data = Failure }
                                , documents = singleton ( old.path, Delete )
                            }

                        Just new ->
                            { upd
                                | value =
                                    Document { old | data = Committing new }
                                , documents =
                                    singleton
                                        ( old.path, whole <| Committing new )
                            }
            in
            case old.data of
                Loading ->
                    { upd
                        | laters = alter enc f
                        , requests = singleton old.path
                    }

                Failure ->
                    case f <| Nothing of
                        Nothing ->
                            upd

                        Just new ->
                            { upd
                                | value =
                                    Document { old | data = Committing new }
                                , documents =
                                    singleton
                                        ( old.path, whole <| Committing new )
                            }

                Committing a ->
                    mod a

                UpToDate a ->
                    mod a


default : Encoder (Document r) -> r -> Updater (Document r)
default enc new =
    Updater <|
        \(Document old) ->
            let
                whole d =
                    Whole <|
                        Encode.coerce <|
                            enc <|
                                Document { old | data = d }

                upd =
                    noUpdates <| Document old
            in
            case old.data of
                Loading ->
                    { upd | laters = default enc new }

                Failure ->
                    { upd
                        | value =
                            Document { old | data = Committing new }
                        , documents =
                            singleton
                                ( old.path, whole <| Committing new )
                    }

                _ ->
                    upd



-- mergeUpdate : Updates a -> Updates a -> Updates a
-- mergeUpdate ux uy =
--     { documents = Array.append ux.documents uy.documents
--     , collections = Array.append ux.collections uy.collections
--     , laters =
--         Updater <|
--             \a ->
--                 mergeUpdate (runUpdater ux.laters a) (runUpdater uy.laters a)
--     }


both : Updater a -> Updater a -> Updater a
both (Updater f) (Updater g) =
    Updater <|
        \a ->
            let
                ux =
                    f a

                uy =
                    g ux.value
            in
            { value = uy.value
            , documents = Array.append ux.documents uy.documents
            , collections = Array.append ux.collections uy.collections
            , laters = both ux.laters uy.laters
            , requests = Array.append ux.requests uy.requests
            }


mapUpdates : (a -> b) -> (Updater a -> Updater b) -> Updates a -> Updates b
mapUpdates f uf { value, documents, collections, laters, requests } =
    { value = f value
    , documents = documents
    , collections = collections
    , laters = uf laters
    , requests = requests
    }


addLaters : Updater a -> Updates a -> Updates a
addLaters f upd =
    { upd | laters = both f upd.laters }
