module Firestore.Path.Id exposing (..)


type Id a
    = Id String


type alias SomeId =
    String


type alias SelfId =
    String


null : Id x
null =
    Id ""


coerce : Id a -> Id b
coerce (Id str) =
    Id str


selfId : SelfId -> Id x
selfId =
    Id


selfIds : List SelfId -> List (Id x)
selfIds =
    List.map Id


fromString : String -> Id x
fromString =
    Id


unId : Id a -> String
unId (Id str) =
    str


self : { r | id : String } -> Id { r | id : String }
self r =
    Id r.id
