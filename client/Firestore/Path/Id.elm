module Firestore.Path.Id exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict


type Id a
    = Id String


type alias SomeId =
    String


type alias SelfId =
    String


type IdMap x a
    = IdMap (Dict SomeId a)


coerce : Id a -> Id b
coerce (Id str) =
    Id str


fromString : String -> Id x
fromString =
    Id


unId : Id a -> String
unId (Id str) =
    str


self : { r | id : String } -> Id { r | id : String }
self r =
    Id r.id


empty : IdMap x a
empty =
    IdMap Dict.empty


singleton : Id x -> a -> IdMap x a
singleton (Id id) a =
    IdMap <| Dict.singleton id a


get : Id x -> IdMap x a -> Maybe a
get (Id id) (IdMap d) =
    Dict.get id d


member : Id x -> IdMap x a -> Bool
member (Id id) (IdMap d) =
    Dict.member id d


size : IdMap x a -> Int
size (IdMap d) =
    Dict.size d


insert : Id x -> a -> IdMap x a -> IdMap x a
insert (Id id) a (IdMap d) =
    IdMap <| Dict.insert id a d


modify : Id x -> (a -> a) -> IdMap x a -> IdMap x a
modify (Id id) f (IdMap d) =
    IdMap <| Dict.update id (Maybe.map f) d


toList : IdMap x a -> List ( Id x, a )
toList (IdMap d) =
    Dict.toList d |> List.map (Tuple.mapFirst Id)


fromList : List ( Id x, a ) -> IdMap x a
fromList =
    List.map (Tuple.mapFirst unId) >> Dict.fromList >> IdMap


keys : IdMap x a -> List (Id x)
keys (IdMap d) =
    Dict.keys d |> List.map Id


items : IdMap x a -> List a
items (IdMap d) =
    Dict.values d


filter : (Id x -> a -> Bool) -> IdMap x a -> IdMap x a
filter f (IdMap d) =
    IdMap <| Dict.filter (\id -> f <| Id id) d


any : (a -> Bool) -> IdMap x a -> Bool
any f (IdMap d) =
    Dict.any (\_ -> f) d


fromDict : Dict String a -> IdMap x a
fromDict =
    IdMap


toDict : IdMap x a -> Dict String a
toDict (IdMap d) =
    d


map : (a -> b) -> IdMap x a -> IdMap x b
map f (IdMap d) =
    IdMap <| Dict.map (\_ -> f) d


groupBy : (a -> Id x) -> List a -> IdMap x (List a)
groupBy f =
    Dict.groupBy (f >> unId) >> IdMap
