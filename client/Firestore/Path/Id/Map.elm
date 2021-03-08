module Firestore.Path.Id.Map exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Firestore.Path.Id exposing (..)


type Map x a
    = Map (Dict SomeId a)


empty : Map x a
empty =
    Map Dict.empty


singleton : Id x -> a -> Map x a
singleton (Id id) a =
    Map <| Dict.singleton id a


get : Id x -> Map x a -> Maybe a
get (Id id) (Map d) =
    Dict.get id d


member : Id x -> Map x a -> Bool
member (Id id) (Map d) =
    Dict.member id d


size : Map x a -> Int
size (Map d) =
    Dict.size d


insert : Id x -> a -> Map x a -> Map x a
insert (Id id) a (Map d) =
    Map <| Dict.insert id a d


modify : Id x -> (a -> a) -> Map x a -> Map x a
modify (Id id) f (Map d) =
    Map <| Dict.update id (Maybe.map f) d


toList : Map x a -> List ( Id x, a )
toList (Map d) =
    Dict.toList d |> List.map (Tuple.mapFirst Id)


fromList : List ( Id x, a ) -> Map x a
fromList =
    List.map (Tuple.mapFirst unId) >> Dict.fromList >> Map


keys : Map x a -> List (Id x)
keys (Map d) =
    Dict.keys d |> List.map Id


items : Map x a -> List a
items (Map d) =
    Dict.values d


filter : (Id x -> a -> Bool) -> Map x a -> Map x a
filter f (Map d) =
    Map <| Dict.filter (\id -> f <| Id id) d


any : (a -> Bool) -> Map x a -> Bool
any f (Map d) =
    Dict.any (\_ -> f) d


fromDict : Dict String a -> Map x a
fromDict =
    Map


toDict : Map x a -> Dict String a
toDict (Map d) =
    d


map : (a -> b) -> Map x a -> Map x b
map f (Map d) =
    Map <| Dict.map (\_ -> f) d


groupBy : (a -> Id x) -> List a -> Map x (List a)
groupBy f =
    Dict.groupBy (f >> unId) >> Map
