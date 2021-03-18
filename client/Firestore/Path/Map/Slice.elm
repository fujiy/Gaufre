module Firestore.Path.Map.Slice exposing (..)

import Array exposing (Array)
import Dict
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id(..))
import Firestore.Path.Map as Map exposing (..)
import Json.Encode as Encode exposing (Value)


type alias Slice p q =
    q -> Array p


zero : Slice p p
zero p =
    Array.fromList [ p ]


nothing : Slice p q
nothing _ =
    Array.empty


both : Slice p q -> Slice p q -> Slice p q
both sx sy p =
    Array.append (sx p) (sy p)


compose : Slice o p -> Slice p q -> Slice o q
compose sop spq =
    spq >> Array.foldl (sop >> Array.append) Array.empty


col : Id x -> Slice (Map a) (Col a)
col (Id id) c =
    singleton <| Root <| Dict.singleton id c


doc : Id x -> Slice (Col a) (Doc a)
doc (Id id) d =
    singleton <| Col Nothing (Dict.singleton id d) Dict.empty


query : String -> String -> Value -> Slice (Col a) (Col a)
query field op value c =
    let
        v =
            Encode.encode 0 value
    in
    singleton <| Col Nothing Dict.empty (Dict.singleton ( field, op, v ) c)


subCol : Id x -> Slice (Doc a) (Col a)
subCol (Id id) c =
    singleton <| Doc Nothing (Dict.singleton id c)


docItem : Slice (Doc a) a
docItem a =
    singleton <| Doc (Just a) Dict.empty


colItem : Slice (Col a) a
colItem a =
    singleton <| Col (Just a) Dict.empty Dict.empty


addCol : Slice x (Col a) -> Col a -> Slice x (Col a)
addCol s ac c =
    Array.append (s ac) (s c)


addDoc : Slice x (Doc a) -> Doc a -> Slice x (Doc a)
addDoc s ad d =
    Array.append (s ad) (s d)


addSubCol : Slice x (Col a) -> Col a -> Slice x (Col a)
addSubCol s ac c =
    Array.append (s ac) (s c)


also : Slice (Map a) x -> x -> Slice (Map a) y -> Slice (Map a) y
also sx x sy m =
    Array.append (sx x) (sy m)


toMap : (a -> a -> a) -> a -> Slice (Map a) a -> Map a
toMap merger a s =
    s a |> Array.foldl (merge merger) empty


toMapDoc : (a -> a -> a) -> a -> Slice (Map a) (Doc a) -> Map a
toMapDoc merger a s =
    s (docRootItem a) |> Array.foldl (merge merger) empty


toMapCol : (a -> a -> a) -> a -> Slice (Map a) (Col a) -> Map a
toMapCol merger a s =
    s (colRootItem a) |> Array.foldl (merge merger) empty


singletonDoc : Path.Path -> Slice (Map a) (Doc a)
singletonDoc p d =
    case p of
        Path.Root ->
            singleton empty

        Path.RootCol id pc ->
            singleton <|
                Root (Dict.singleton id <| getSingleCol <| singletonDocCol pc d)


singletonDocDoc : Path.Doc -> Slice (Doc a) (Doc a)
singletonDocDoc pd d =
    case pd of
        Path.Doc ->
            singleton d

        Path.SubCol id pc ->
            singleton <|
                Doc Nothing
                    (Dict.singleton id <| getSingleCol <| singletonDocCol pc d)


singletonDocCol : Path.Col -> Slice (Col a) (Doc a)
singletonDocCol pc d =
    case pc of
        Path.Col ->
            nothing d

        Path.SubDoc id pd ->
            singleton <|
                Col Nothing
                    (Dict.singleton id <| getSingleDoc <| singletonDocDoc pd d)
                    Dict.empty

        Path.Query field op value pc_ ->
            singleton <|
                Col Nothing
                    Dict.empty
                    (Dict.singleton (queryKey field op value) <|
                        getSingleCol <|
                            singletonDocCol pc_ d
                    )


getSingleDoc : Array (Doc a) -> Doc a
getSingleDoc ds =
    Array.get 0 ds
        |> Maybe.withDefault emptyDoc


getSingleCol : Array (Col a) -> Col a
getSingleCol cs =
    Array.get 0 cs
        |> Maybe.withDefault emptyCol


singleton : a -> Array a
singleton a =
    Array.fromList [ a ]
