module Firestore.Update exposing (..)

import Firestore.Desc exposing (DocumentDesc(..))
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path
import Firestore.Remote as Remote exposing (Remote(..))
import List.Extra as List
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


set : Lens a (Document s r) -> DocumentDesc s r -> r -> Updater a
set (Lens _ upd) (DocumentDesc d) r =
    upd (Set <| d.encoder <| Committing r) (Document d.empty (Committing r))


delete : Lens a (Document s r) -> DocumentDesc s r -> Updater a
delete (Lens _ upd) (DocumentDesc d) =
    upd (Set <| d.encoder Failure) (Document d.empty Failure)


type Alter a
    = Update a
    | NoChange
    | Delete


alter :
    Lens a (Document s r)
    -> DocumentDesc s r
    -> (Maybe r -> Alter r)
    -> Updater a
alter (Lens acc upd) (DocumentDesc d) f =
    let
        updater _ =
            Updater <|
                \a ->
                    let
                        (Accessor paths rd) =
                            acc a

                        s =
                            Remote.toMaybe rd
                                |> Maybe.unwrap d.empty
                                    (\(Document s_ _) -> s_)

                        update mr =
                            let
                                ( newR, mu ) =
                                    case f mr of
                                        Update r ->
                                            ( Committing r
                                            , Just <|
                                                Set <|
                                                    d.encoder <|
                                                        Committing r
                                            )

                                        NoChange ->
                                            ( Remote.fromMaybe mr, Nothing )

                                        Delete ->
                                            ( Failure, Just Internal.Delete )
                            in
                            case mu of
                                Just u ->
                                    upd u (Document s newR)
                                        |> flip runUpdater a

                                Nothing ->
                                    Internal.noUpdates a
                    in
                    case
                        Remote.andThen (\(Document _ rr) -> rr) rd
                    of
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


modify : Lens a (Document s r) -> DocumentDesc s r -> (r -> r) -> Updater a
modify l d f =
    alter l d <|
        Maybe.unwrap NoChange
            (\r ->
                let
                    new =
                        f r
                in
                if new == r then
                    NoChange

                else
                    Update new
            )


default : Lens a (Document s r) -> DocumentDesc s r -> r -> Updater a
default l d r =
    alter l d <| Maybe.unwrap (Update r) (always NoChange)


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


list : (b -> Updater a) -> List b -> Updater (List a)
list updater bs =
    Updater <|
        \xs ->
            List.foldr
                (\( a, b ) upds ->
                    let
                        upd =
                            runUpdater (updater b) a
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
                        both upds.afterwards <| list updater bs
                    }
                )
                { value = []
                , updates = Path.empty
                , requests = Path.empty
                , afterwards = noUpdater
                }
                (List.zip xs bs)
