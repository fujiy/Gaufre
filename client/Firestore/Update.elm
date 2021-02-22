module Firestore.Update exposing (..)

-- import Firestore.Decode as Decode exposing (Decoder)
-- import Firestore.Encode as Encode exposing (Encoder)

import Array exposing (Array)
import Dict
import Firestore.Desc exposing (DocumentDesc(..))
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path exposing (PathMap)
import Firestore.Remote as Remote exposing (Remote(..))
import GDrive exposing (request)
import Json.Decode
import Maybe.Extra as Maybe
import Util exposing (..)


type alias Updater a =
    Internal.Updater a


all : List (Updater a) -> Updater a
all upds =
    List.foldr both noUpdater upds


none : Updater a
none =
    noUpdater



-- firestore : Updater r -> Updater (Firestore r)
-- firestore upd =
--     Updater <|
--         \(Firestore fs) ->
--             runUpdater upd fs.data
--                 |> mapUpdates (\r -> Firestore { fs | data = r }) firestore
-- collection :
--     Lens r (Collection a)
--     -> Updater (Collection a)
--     -> Updater r
-- collection lens upd =
--     Updater <|
--         \r ->
--             lens.get r
--                 |> runUpdater upd
--                 |> mapUpdates
--                     (flip lens.set r)
--                     (collection lens)
-- doc : Id -> Updater (Document r) -> Updater (Collection r)
-- doc id (Updater f) =
--     Updater <|
--         \(Collection c) ->
--             Dict.get id c.documents
--                 |> Maybe.withDefault (loadingDocument (sub c.path id))
--                 |> f
--                 |> mapUpdates
--                     (\d ->
--                         Collection <|
--                             { c | documents = Dict.insert id d c.documents }
--                     )
--                     (doc id)


set : Lens a (Document r) -> DocumentDesc r -> r -> Updater a
set (Lens _ upd) (DocumentDesc { encoder }) r =
    upd (Set <| encoder <| Committing r) (Document (Committing r))


delete : Lens a (Document r) -> DocumentDesc r -> Updater a
delete (Lens _ upd) (DocumentDesc { encoder }) =
    upd (Set <| encoder Failure) (Document Failure)


type Alter a
    = Update a
    | NoChange
    | Delete


alter :
    Lens a (Document r)
    -> DocumentDesc r
    -> (Maybe r -> Alter r)
    -> Updater a
alter (Lens acc upd) (DocumentDesc { encoder }) f =
    let
        updater _ =
            Updater <|
                \a ->
                    let
                        (Accessor paths rd) =
                            acc a

                        update mr =
                            let
                                ( newR, mu ) =
                                    case f mr of
                                        Update r ->
                                            ( Committing r
                                            , Just <|
                                                Set <|
                                                    encoder <|
                                                        Committing r
                                            )

                                        NoChange ->
                                            ( Remote.fromMaybe mr, Nothing )

                                        Delete ->
                                            ( Failure, Just Internal.Delete )
                            in
                            case mu of
                                Just u ->
                                    upd u (Document newR)
                                        |> flip runUpdater a

                                Nothing ->
                                    Internal.noUpdates a
                    in
                    case Remote.andThen (\(Document rr) -> rr) rd of
                        Loading ->
                            { value = a
                            , updates = Path.empty
                            , requests = paths
                            , afterwards = updater ()
                            }

                        Failure ->
                            update Nothing

                        UpToDate r ->
                            update <| Just r

                        Committing r ->
                            update <| Just r
    in
    updater ()


modify : Lens a (Document r) -> DocumentDesc r -> (r -> r) -> Updater a
modify l d f =
    alter l d <| Maybe.unwrap NoChange (f >> Update)


default : Lens a (Document r) -> DocumentDesc r -> r -> Updater a
default l d r =
    alter l d <| Maybe.unwrap (Update r) (always NoChange)



-- add : Encoder (Document r) -> Document r -> Updater (Collection r)
-- add enc d =
--     Updater <|
--         \(Collection old) ->
--             let
--                 upd =
--                     noUpdates <| Collection old
--             in
--             { upd
--                 | collections =
--                     Array.fromList
--                         [ ( old.path, Add <| Encode.coerce <| enc d ) ]
--             }
-- -- field : String -> Value () -> Updater (Document r)
-- -- field name v =
-- --     Updater <|
-- --         \(Document d) ->
-- --             { noUpdates
-- --                 | documents =
-- --                     Array.fromList
-- --                         [ ( d.path, Field name v ) ]
-- --             }
-- default : Encoder (Document r) -> r -> Updater (Document r)
-- default enc new =
--     Updater <|
--         \(Document old) ->
--             let
--                 whole d =
--                     Whole <|
--                         Encode.coerce <|
--                             enc <|
--                                 Document { old | data = d }
--                 upd =
--                     noUpdates <| Document old
--             in
--             case old.data of
--                 Loading ->
--                     { upd | laters = default enc new }
--                 Failure ->
--                     { upd
--                         | value =
--                             Document { old | data = Committing new }
--                         , documents =
--                             Array.fromList
--                                 [ ( old.path, whole <| Committing new ) ]
--                     }
--                 _ ->
--                     upd


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
            , updates = Path.merge always uy.updates ux.updates
            , requests = Path.append ux.requests uy.requests
            , afterwards = both ux.afterwards uy.afterwards
            }



-- mapUpdates : (a -> b) -> (Updater a -> Updater b) -> Updates a -> Updates b
-- mapUpdates f uf { value, documents, collections, laters, requests } =
--     { value = f value
--     , documents = documents
--     , collections = collections
--     , laters = uf laters
--     , requests = requests
--     }
-- addLaters : Updater a -> Updates a -> Updates a
-- addLaters f upd =
--     { upd | laters = both f upd.laters }
