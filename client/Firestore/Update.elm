module Firestore.Update exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Firestore.Desc exposing (DocumentDesc(..))
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path
import Firestore.Path.Map as PathMap
import Firestore.Path.Map.Slice as Slice
import Firestore.Remote as Remote exposing (Remote(..))
import List.Extra as List
import Maybe.Extra as Maybe
import Util exposing (..)


type Updater a
    = Updater (a -> Updates a)


type alias Updates a =
    { value : a
    , requests : PathMap.Map Request
    , afterwards : Updater a
    }


runUpdater : Updater a -> a -> Updates a
runUpdater (Updater f) =
    f


all : List (Updater a) -> Updater a
all =
    List.foldr both none


none : Updater a
none =
    Updater noUpdates


noUpdates : a -> Updates a
noUpdates a =
    Updates a PathMap.empty none


fromDoc : Internal.Updater Root Doc a -> Updater a
fromDoc (Internal.Updater f) =
    Updater <|
        \a ->
            let
                upd =
                    f a
            in
            { value = upd.value
            , requests =
                Slice.toMapDoc mergeRequest Get upd.requests
            , afterwards = fromDoc upd.afterwards
            }


set :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> r
    -> Updater a
set (Lens l) (DocumentDesc d) r =
    l.update
        (Set <| d.encoder <| Committing r)
        (Document d.empty (Committing r))
        |> fromDoc


delete :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> Updater a
delete (Lens l) (DocumentDesc d) =
    l.update (Set <| d.encoder Failure) (Document d.empty Failure)
        |> fromDoc


type Alter a
    = Update a
    | NoChange
    | Delete


alter :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> (Maybe r -> Alter r)
    -> Updater a
alter (Lens l) (DocumentDesc d) f =
    let
        updater _ =
            Updater <|
                \a ->
                    let
                        (Accessor paths rd) =
                            l.access a

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
                                    l.update u (Document s newR)
                                        |> fromDoc
                                        |> flip runUpdater a

                                Nothing ->
                                    noUpdates a
                    in
                    case
                        Remote.andThen (\(Document _ rr) -> rr) rd
                    of
                        Loading ->
                            { value = a
                            , requests =
                                Slice.toMapDoc mergeRequest Get paths
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


modify :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> (r -> r)
    -> Updater a
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


default : Lens Root a Doc (Document s r) -> DocumentDesc s r -> r -> Updater a
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
            , requests = PathMap.merge mergeRequest ux.requests uy.requests
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
                    , requests =
                        PathMap.merge mergeRequest upd.requests upds.requests
                    , afterwards = both upds.afterwards <| list updater bs
                    }
                )
                (noUpdates [])
                (List.zip xs bs)


array : (b -> Updater a) -> Array b -> Updater (Array a)
array updater bs =
    Updater <|
        \xs ->
            Array.foldl
                (\( a, b ) upds ->
                    let
                        upd =
                            runUpdater (updater b) a
                    in
                    { value = Array.push upd.value upds.value
                    , requests =
                        PathMap.merge mergeRequest upd.requests upds.requests
                    , afterwards = both upds.afterwards <| array updater bs
                    }
                )
                (noUpdates Array.empty)
                (Array.zip xs bs)
