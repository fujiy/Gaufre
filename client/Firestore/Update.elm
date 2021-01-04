module Firestore.Update exposing (..)

import Array exposing (Array)
import Dict
import Firestore.Decode as Decode exposing (Decoder)
import Firestore.Encode as Encode exposing (Encoder)
import Firestore.Internal as Internal exposing (..)
import Json.Decode
import Maybe.Extra as Maybe
import Util exposing (..)


type alias Updater a =
    Internal.Updater a


updates : List (Updater a) -> Updater a
updates upds =
    List.foldr both noUpdater upds


firestore : Updater r -> Updater (Firestore r)
firestore upd =
    Updater <|
        \(Firestore fs) ->
            runUpdater upd fs.data
                |> mapUpdates (\r -> Firestore { fs | data = r }) firestore


collection :
    (r -> Collection a)
    -> (r -> Collection a -> r)
    -> Updater (Collection a)
    -> Updater r
collection getter setter upd =
    Updater <|
        \r ->
            getter r
                |> runUpdater upd
                |> mapUpdates
                    (setter r)
                    (collection getter setter)


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


update : Encoder (Document r) -> (Maybe r -> Maybe r) -> Updater (Document r)
update enc f =
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

                modify a =
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
                    { upd | laters = update enc f }

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
                    modify a

                UpToDate a ->
                    modify a


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
            }


mapUpdates : (a -> b) -> (Updater a -> Updater b) -> Updates a -> Updates b
mapUpdates f uf { value, documents, collections, laters } =
    { value = f value
    , documents = documents
    , collections = collections
    , laters = uf laters
    }


addLaters : Updater a -> Updates a -> Updates a
addLaters f upd =
    { upd | laters = both f upd.laters }
