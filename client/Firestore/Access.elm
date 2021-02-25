module Firestore.Access exposing (..)

import Array exposing (Array)
import Firestore.Internal as Internal exposing (..)
import Firestore.Path as Path exposing (Path, Paths)
import Firestore.Remote as Remote exposing (Remote(..))
import Maybe.Extra as Maybe
import Util exposing (..)


type alias Accessor r a =
    Internal.Accessor r a


success : a -> Accessor r a
success a =
    Accessor Path.empty (UpToDate a)


failure : Accessor r a
failure =
    Accessor Path.empty Failure


map : (a -> b) -> Accessor r a -> Accessor r b
map f (Accessor paths ra) =
    Accessor paths <| Remote.map f ra


map2 : (a -> b -> c) -> Accessor r a -> Accessor r b -> Accessor r c
map2 f (Accessor aps ra) (Accessor bps rb) =
    Accessor (Path.append aps bps) <| Remote.map2 f ra rb


map3 :
    (a -> b -> c -> d)
    -> Accessor r a
    -> Accessor r b
    -> Accessor r c
    -> Accessor r d
map3 f (Accessor aps ra) (Accessor bps rb) (Accessor cps rc) =
    Accessor (Path.append aps <| Path.append bps cps) <| Remote.map3 f ra rb rc


mapRemote : (Remote a -> Remote b) -> Accessor r a -> Accessor r b
mapRemote f (Accessor paths ra) =
    Accessor paths <| f ra


unremote : Remote (Accessor r a) -> Accessor r a
unremote ra =
    case ra of
        Failure ->
            failure

        Loading ->
            Accessor Path.empty Loading

        Committing (Accessor paths (UpToDate a)) ->
            Accessor paths (Committing a)

        Committing aa ->
            aa

        UpToDate aa ->
            aa


toRemote : Accessor r a -> Remote (Accessor r a)
toRemote (Accessor paths ra) =
    case ra of
        Failure ->
            Failure

        Loading ->
            Loading

        Committing a ->
            Committing <| Accessor paths <| UpToDate a

        UpToDate a ->
            UpToDate <| Accessor paths <| UpToDate a


remote : Accessor r a -> Accessor r (Remote a)
remote =
    mapRemote UpToDate


andThen : (a -> Accessor r b) -> Accessor r a -> Accessor r b
andThen f (Accessor paths ra) =
    Remote.andThen (f >> toRemote) ra
        |> unremote
        |> requires paths


andThen2 : (a -> b -> Accessor r c) -> Accessor r a -> Accessor r b -> Accessor r c
andThen2 f (Accessor pas ra) (Accessor pbs rb) =
    Remote.andThen2 (\a b -> f a b |> toRemote) ra rb
        |> unremote
        |> requires (Path.append pas pbs)


for : (a -> Accessor r b) -> Accessor r (List a) -> Accessor r (List b)
for f ac =
    map (List.map f >> list) ac |> andThen identity


forArray : (a -> Accessor r b) -> Accessor r (Array a) -> Accessor r (Array b)
forArray f ac =
    map (Array.map f >> array) ac |> andThen identity


list : List (Accessor r a) -> Accessor r (List a)
list =
    List.foldr (map2 (::)) (success [])


array : Array (Accessor r a) -> Accessor r (Array a)
array =
    Array.toList >> list >> map Array.fromList


maps : (List a -> b) -> List (Accessor r a) -> Accessor r b
maps f =
    map f << list



-- maybe : Maybe (Accessor r a) -> Accessor (Maybe a)
-- maybe m =
--     case m of
--         Nothing ->
--             failure
--         Just (Accessor paths ra) ->
--             Accessor paths <| UpToDate ra


maybe : Accessor r a -> Accessor r (Maybe a)
maybe =
    mapRemote <| Remote.toMaybe >> UpToDate


fromJust : Accessor r (Maybe a) -> Accessor r a
fromJust =
    mapRemote Remote.unmaybe


access : Lens a r -> a -> Accessor a r
access (Lens acc _) a =
    acc a


accessMap : Lens a r -> a -> (r -> b) -> Accessor a b
accessMap (Lens acc _) a f =
    map f <| acc a


accessMapMaybe : Lens a r -> a -> (Maybe r -> b) -> Accessor a b
accessMapMaybe (Lens acc _) a f =
    map f <| maybe <| acc a


thenAccess : Lens a r -> Accessor d a -> Accessor d r
thenAccess l (Accessor paths a) =
    let
        (Accessor paths_ b) =
            Remote.map (access l) a |> unremote
    in
    Accessor (Path.joinSub paths paths_) b


require : Path -> Accessor r a -> Accessor r a
require path (Accessor paths ra) =
    Accessor (Path.push path paths) ra


requires : Paths -> Accessor r a -> Accessor r a
requires paths_ (Accessor paths ra) =
    Accessor (Path.append paths_ paths) ra
