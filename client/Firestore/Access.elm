module Firestore.Access exposing (..)

import Array exposing (Array)
import Firestore.Internal as I exposing (..)
import Firestore.Path as Path exposing (Path)
import Firestore.Path.Map as PathMap
import Firestore.Path.Map.Slice as Slice exposing (..)
import Firestore.Remote as Remote exposing (Remote(..))
import Maybe.Extra as Maybe
import Util exposing (..)


type alias Accessor r a =
    I.Accessor Root Item r a


just : a -> I.Accessor x x r a
just a =
    I.Accessor Slice.zero (UpToDate a)


success : a -> I.Accessor p q r a
success a =
    I.Accessor Slice.nothing (UpToDate a)


failure : I.Accessor p q r a
failure =
    I.Accessor Slice.nothing Failure


map : (a -> b) -> I.Accessor p q r a -> I.Accessor p q r b
map f (I.Accessor s ra) =
    I.Accessor s <| Remote.map f ra


map2 :
    (a -> b -> c)
    -> I.Accessor p q r a
    -> I.Accessor p q r b
    -> I.Accessor p q r c
map2 f (I.Accessor sa ra) (I.Accessor sb rb) =
    I.Accessor (Slice.both sa sb) <| Remote.map2 f ra rb


map3 :
    (a -> b -> c -> d)
    -> I.Accessor p q r a
    -> I.Accessor p q r b
    -> I.Accessor p q r c
    -> I.Accessor p q r d
map3 f (I.Accessor sa ra) (I.Accessor sb rb) (I.Accessor sc rc) =
    I.Accessor (Slice.both sa <| Slice.both sb sc) <| Remote.map3 f ra rb rc


mapRemote : (Remote a -> Remote b) -> I.Accessor p q r a -> I.Accessor p q r b
mapRemote f (I.Accessor s ra) =
    I.Accessor s <| f ra


unremote : Remote (I.Accessor p q r a) -> I.Accessor p q r a
unremote ra =
    case ra of
        Failure ->
            I.Accessor Slice.nothing Failure

        Loading ->
            I.Accessor Slice.nothing Loading

        Committing (I.Accessor s (UpToDate a)) ->
            I.Accessor s (Committing a)

        Committing aa ->
            aa

        UpToDate aa ->
            aa


toRemote : I.Accessor p q r a -> Remote (I.Accessor p q r a)
toRemote (I.Accessor s ra) =
    case ra of
        Failure ->
            Failure

        Loading ->
            Loading

        Committing a ->
            Committing <| I.Accessor s <| UpToDate a

        UpToDate a ->
            UpToDate <| I.Accessor s <| UpToDate a


remote : I.Accessor p q r a -> I.Accessor p q r (Remote a)
remote =
    mapRemote UpToDate


join : I.Accessor p q r (I.Accessor p q r a) -> I.Accessor p q r a
join (I.Accessor sp raa) =
    case raa of
        Failure ->
            I.Accessor sp Failure

        Loading ->
            I.Accessor sp Loading

        Committing (I.Accessor ss (UpToDate a)) ->
            I.Accessor (Slice.both sp ss) (Committing a)

        Committing (I.Accessor ss ra) ->
            I.Accessor (Slice.both sp ss) ra

        UpToDate (I.Accessor ss ra) ->
            I.Accessor (Slice.both sp ss) ra


andThen :
    (a -> I.Accessor p q r b)
    -> I.Accessor p q r a
    -> I.Accessor p q r b
andThen f =
    map f >> join


andThen2 :
    (a -> b -> I.Accessor p q r c)
    -> I.Accessor p q r a
    -> I.Accessor p q r b
    -> I.Accessor p q r c
andThen2 f ra rb =
    map2 f ra rb |> join


for :
    (a -> I.Accessor p q r b)
    -> I.Accessor p q r (List a)
    -> I.Accessor p q r (List b)
for f ac =
    map (List.map f >> list) ac |> andThen identity


forArray :
    (a -> I.Accessor p q r b)
    -> I.Accessor p q r (Array a)
    -> I.Accessor p q r (Array b)
forArray f ac =
    map (Array.map f >> array) ac |> andThen identity


list : List (I.Accessor p q r a) -> I.Accessor p q r (List a)
list =
    List.foldr (map2 (::)) (success [])


array : Array (I.Accessor p q r a) -> I.Accessor p q r (Array a)
array =
    Array.foldr (map2 Array.push) (success Array.empty)


maps : (List a -> b) -> List (I.Accessor p q r a) -> I.Accessor p q r b
maps f =
    map f << list


maybe : I.Accessor p q r a -> I.Accessor p q r (Maybe a)
maybe =
    mapRemote <| Remote.toMaybe >> UpToDate


fromJust : I.Accessor p q r (Maybe a) -> I.Accessor p q r a
fromJust =
    mapRemote Remote.unmaybe


access : Lens Root a q r -> a -> I.Accessor Root q a r
access (Lens l) =
    l.access



-- withPaths : I.Accessor p q r a -> I.Accessor p q r ( List Path, a )
-- withPaths (I.Accessor s ra) =
--     I.Accessor s <|
--         Remote.map
--             (\a -> ( PathMap.toList s |> List.map Tuple.first, a ))
--             ra


accessMap : Lens p a q r -> a -> (r -> b) -> I.Accessor p q a b
accessMap (Lens l) a f =
    map f <| l.access a


accessMapMaybe : Lens p a q r -> a -> (Maybe r -> b) -> I.Accessor p q a b
accessMapMaybe (Lens l) a f =
    map f <| maybe <| l.access a


thenAccess : Lens Root a q r -> I.Accessor Root q d a -> I.Accessor Root q d r
thenAccess l (I.Accessor rs a) =
    let
        (I.Accessor ss b) =
            Remote.map (access l) a |> unremote
    in
    I.Accessor (Slice.both rs ss) b
