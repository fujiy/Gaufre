module Firestore.Update exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Dict
import Firestore.Access as Access
import Firestore.Desc exposing (DocumentDesc(..))
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (SelfId)
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Map as PathMap
import Firestore.Path.Map.Slice as Slice
import Firestore.Remote as Remote exposing (Remote(..))
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import Util exposing (..)


type Updater a msg
    = Updater (a -> Updates a msg)


type alias Updates a msg =
    { value : a
    , requests : PathMap.Map Request
    , command : Cmd msg
    , afterwards : Updater a msg
    }


runUpdater : Updater a msg -> a -> Updates a msg
runUpdater (Updater f) =
    f


all : List (Updater a msg) -> Updater a msg
all =
    List.foldr both none


none : Updater a msg
none =
    Updater noUpdates_


noUpdates_ : a -> Updates a msg
noUpdates_ a =
    Updates a PathMap.empty Cmd.none none


noUpdates : a -> msg -> Updates a msg
noUpdates a msg =
    Updates a PathMap.empty (Task.perform identity <| Task.succeed msg) none


succeed : msg -> Updater a msg
succeed msg =
    command <| \_ -> Task.perform identity <| Task.succeed msg


withCommand : msg -> Updater a msg -> Updater a msg
withCommand msg (Updater f) =
    Updater <|
        \a ->
            let
                upd =
                    f a
            in
            if
                PathMap.filterMap toUpdateRequest upd.requests
                    |> PathMap.isEmpty
            then
                { upd | afterwards = withCommand msg upd.afterwards }

            else
                { upd
                    | command =
                        Cmd.batch
                            [ upd.command
                            , Task.perform identity <| Task.succeed msg
                            ]
                }


command : (a -> Cmd msg) -> Updater a msg
command cmd =
    Updater <|
        \a ->
            { value = a
            , requests = PathMap.empty
            , command = cmd a
            , afterwards = none
            }


batch : List (a -> Cmd msg) -> Updater a msg
batch cmds =
    command <| \a -> List.map (\f -> f a) cmds |> Cmd.batch


map : (a -> msg) -> Updater x a -> Updater x msg
map f upd =
    Updater <|
        \a ->
            let
                upds =
                    runUpdater upd a
            in
            { value = upds.value
            , requests = upds.requests
            , command = Cmd.map f upds.command
            , afterwards = map f upds.afterwards
            }


for : List a -> (a -> Updater b msg) -> Updater b msg
for xs f =
    List.map f xs |> all


fromDoc : Internal.Updater Root Doc a -> Updater a msg
fromDoc (Internal.Updater f) =
    Updater <|
        \a ->
            let
                upd =
                    f a
            in
            { value = upd.value
            , requests =
                Slice.toMapDoc mergeRequest noRequest upd.requests
            , command = Cmd.none
            , afterwards = fromDoc upd.afterwards
            }


set :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> r
    -> Updater a msg
set (Lens l) (DocumentDesc d) r =
    l.update
        (setRequest <| d.encoder <| Committing r)
        (Document d.empty (Committing r))
        |> fromDoc


delete :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> Updater a msg
delete (Lens l) (DocumentDesc d) =
    l.update deleteRequest (Document d.empty Failure)
        |> fromDoc


type Alter a
    = Update a
    | NoChange
    | Delete


alter :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> (Maybe r -> Alter r)
    -> Updater a msg
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
                                ( newR, mu, new ) =
                                    case f mr of
                                        Update r ->
                                            ( Committing r
                                            , Just <|
                                                setRequest <|
                                                    d.encoder <|
                                                        Committing r
                                            , Just r
                                            )

                                        NoChange ->
                                            ( Remote.fromMaybe mr
                                            , Nothing
                                            , mr
                                            )

                                        Delete ->
                                            ( Failure
                                            , Just deleteRequest
                                            , Nothing
                                            )
                            in
                            case mu of
                                Just u ->
                                    l.update u (Document s newR)
                                        |> fromDoc
                                        |> flip runUpdater a

                                Nothing ->
                                    noUpdates_ a
                    in
                    case
                        Remote.andThen (\(Document _ rr) -> rr) rd
                    of
                        Loading ->
                            { value = a
                            , requests =
                                Slice.toMapDoc mergeRequest getRequest paths
                            , command = Cmd.none
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


alter_ :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> (Maybe r -> Alter r)
    -> Updater a (Maybe r)
alter_ (Lens l) (DocumentDesc d) f =
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
                                ( newR, mu, new ) =
                                    case f mr of
                                        Update r ->
                                            ( Committing r
                                            , Just <|
                                                setRequest <|
                                                    d.encoder <|
                                                        Committing r
                                            , Just r
                                            )

                                        NoChange ->
                                            ( Remote.fromMaybe mr
                                            , Nothing
                                            , mr
                                            )

                                        Delete ->
                                            ( Failure
                                            , Just deleteRequest
                                            , Nothing
                                            )
                            in
                            case mu of
                                Just u ->
                                    l.update u (Document s newR)
                                        |> fromDoc
                                        |> withCommand new
                                        |> flip runUpdater a

                                Nothing ->
                                    noUpdates a new
                    in
                    case
                        Remote.andThen (\(Document _ rr) -> rr) rd
                    of
                        Loading ->
                            { value = a
                            , requests =
                                Slice.toMapDoc mergeRequest getRequest paths
                            , command = Cmd.none
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
    -> Updater a msg
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


modify_ :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> (r -> r)
    -> Updater a (Maybe r)
modify_ l d f =
    alter_ l d <|
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


default :
    Lens Root a Doc (Document s r)
    -> DocumentDesc s r
    -> r
    -> Updater a msg
default l d r =
    alter l d (Maybe.unwrap (Update r) (always NoChange))


add :
    Lens Root a Col (Collection s r)
    -> DocumentDesc s r
    -> (SelfId -> r)
    -> Updater a msg
add (Lens l) (DocumentDesc d) r =
    Updater <|
        \a ->
            let
                (Accessor path rcol) =
                    l.access a

                col =
                    Remote.withDefault
                        (Collection
                            { name =
                                Slice.toMapCol mergeRequest getRequest path
                                    |> PathMap.toList
                                    |> List.head
                                    |> Maybe.map Tuple.first
                                    |> Maybe.andThen Path.getLast
                                    |> Maybe.withDefault Id.null
                            , empty = d.empty
                            , loading = True
                            , docs = IdMap.empty
                            , q = Dict.empty
                            }
                        )
                        rcol

                (Internal.Updater f) =
                    l.update (setRequest <| d.encoder rd) col

                upd =
                    f a

                rd =
                    Committing <| r <| Id.unId Id.null
            in
            { value = upd.value
            , requests =
                Slice.toMapCol mergeRequest noRequest upd.requests
            , command = Cmd.none
            , afterwards = none
            }


both : Updater a msg -> Updater a msg -> Updater a msg
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
            , command = Cmd.batch [ ux.command, uy.command ]
            , afterwards = both ux.afterwards uy.afterwards
            }


andThen : (r -> Access.Accessor r a) -> (a -> Updater r msg) -> Updater r msg
andThen acc upd =
    let
        updater _ =
            Updater <|
                \r ->
                    let
                        (Accessor s ra) =
                            acc r
                    in
                    case ra of
                        Failure ->
                            noUpdates_ r

                        Loading ->
                            { value = r
                            , requests = Slice.toMap mergeRequest getRequest s
                            , command = Cmd.none
                            , afterwards = updater ()
                            }

                        Committing a ->
                            runUpdater (upd a) r

                        UpToDate a ->
                            runUpdater (upd a) r
    in
    updater ()


list : (b -> Updater a msg) -> List b -> Updater (List a) msg
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
                    , command = Cmd.batch [ upds.command, upd.command ]
                    , afterwards = both upds.afterwards <| list updater bs
                    }
                )
                (noUpdates_ [])
                (List.zip xs bs)


array : (b -> Updater a msg) -> Array b -> Updater (Array a) msg
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
                    , command = Cmd.batch [ upds.command, upd.command ]
                    , afterwards = both upds.afterwards <| array updater bs
                    }
                )
                (noUpdates_ Array.empty)
                (Array.zip xs bs)
