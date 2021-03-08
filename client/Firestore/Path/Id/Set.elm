module Firestore.Path.Id.Set exposing (..)

import Firestore.Path.Id exposing (..)
import Set


type Set x
    = Set (Set.Set SomeId)


empty : Set x
empty =
    Set Set.empty


singleton : Id x -> Set x
singleton (Id x) =
    Set <| Set.singleton x


insert : Id x -> Set x -> Set x
insert (Id x) (Set s) =
    Set <| Set.insert x s


remove : Id x -> Set x -> Set x
remove (Id x) (Set s) =
    Set <| Set.remove x s


member : Id x -> Set x -> Bool
member (Id x) (Set s) =
    Set.member x s


size : Set x -> Int
size (Set s) =
    Set.size s


fromList : List (Id x) -> Set x
fromList =
    List.map unId >> Set.fromList >> Set


toList : Set x -> List (Id x)
toList (Set s) =
    Set.toList s |> List.map Id


change : Id x -> Bool -> Bool -> Set x -> Set x
change id select clear s =
    (if select then
        insert

     else
        remove
    )
        id
        (if clear then
            empty

         else
            s
        )
