module Firestore.Path exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Maybe.Extra as Maybe
import Util exposing (orWith)


type alias Id =
    String


type alias Path =
    Array Id


root : Path
root =
    Array.empty


isRoot : Path -> Bool
isRoot =
    Array.isEmpty


topLevel : Id -> Path
topLevel id =
    Array.fromList [ id ]


fromIds : List Id -> Path
fromIds =
    Array.fromList


toIds : Path -> List Id
toIds =
    Array.toList


sub : Path -> Id -> Path
sub path id =
    Array.push id path


getLast : Path -> Maybe Id
getLast p =
    if Array.isEmpty p then
        Nothing

    else
        Array.get (Array.length p - 1) p


type alias Paths =
    PathMap ()


type PathMap a
    = PathMap (Maybe a) (Dict Id (PathMap a))


empty : PathMap a
empty =
    PathMap Nothing Dict.empty


getRootItem : PathMap a -> Maybe a
getRootItem (PathMap ma _) =
    ma


rootItem : a -> PathMap a
rootItem a =
    PathMap (Just a) Dict.empty


singleton : Path -> a -> PathMap a
singleton path a =
    Array.foldr
        (\id m -> PathMap Nothing <| Dict.singleton id m)
        (PathMap (Just a) Dict.empty)
        path


singleton_ : List Id -> a -> PathMap a
singleton_ path a =
    List.foldr
        (\id m -> PathMap Nothing <| Dict.singleton id m)
        (PathMap (Just a) Dict.empty)
        path


isEmpty : PathMap a -> Bool
isEmpty (PathMap ma d) =
    (ma == Nothing)
        && (Dict.isEmpty d || not (Dict.any (\_ -> isEmpty >> not) d))


at : Id -> PathMap a -> PathMap a
at id (PathMap _ d) =
    Dict.get id d |> Maybe.withDefault empty


subMaps : PathMap a -> List ( Id, PathMap a )
subMaps (PathMap _ d) =
    Dict.toList d


subMaps_ : PathMap a -> Dict Id (PathMap a)
subMaps_ (PathMap _ d) =
    d


insert : Path -> a -> PathMap a -> PathMap a
insert path a (PathMap ma d) =
    let
        go id ids =
            Dict.update id <|
                \m ->
                    case m of
                        Nothing ->
                            Just <| singleton_ ids a

                        Just (PathMap ma_ d_) ->
                            case ids of
                                [] ->
                                    Just <| PathMap (Just a) d_

                                id_ :: ids_ ->
                                    Just <| PathMap ma_ <| go id_ ids_ d_
    in
    case Array.toList path of
        [] ->
            PathMap (Just a) d

        id :: ids ->
            PathMap ma <| go id ids d


map : (a -> b) -> PathMap a -> PathMap b
map f (PathMap m d) =
    PathMap (Maybe.map f m) (Dict.map (\_ -> map f) d)


subMap : Id -> PathMap a -> PathMap a
subMap id m =
    PathMap Nothing <| Dict.singleton id m


toList : PathMap a -> List ( Path, a )
toList =
    toList_ >> List.map (\( p, a ) -> ( fromIds p, a ))


toList_ : PathMap a -> List ( List Id, a )
toList_ (PathMap m d) =
    let
        subs =
            Dict.toList d
                |> List.concatMap
                    (\( id, pm ) ->
                        toList_ pm
                            |> List.map
                                (\( ids, a ) -> ( id :: ids, a ))
                    )
    in
    case m of
        Nothing ->
            subs

        Just a ->
            ( [], a ) :: subs


merge : (a -> a -> a) -> PathMap a -> PathMap a -> PathMap a
merge f (PathMap ma da) (PathMap mb db) =
    PathMap
        (orWith f ma mb)
    <|
        Dict.merge Dict.insert
            (\id pl pr ->
                Dict.insert id <| merge f pl pr
            )
            Dict.insert
            da
            db
            Dict.empty


diff :
    PathMap a
    -> PathMap a
    -> ( PathMap a, PathMap a )
diff (PathMap ma da) (PathMap mb db) =
    let
        ( dl_, dr_ ) =
            Dict.merge
                (\id p ( dl, dr ) -> ( Dict.insert id p dl, dr ))
                (\id pl pr ( dl, dr ) ->
                    let
                        ( sdl, sdr ) =
                            diff pl pr
                    in
                    ( if isEmpty sdl then
                        dl

                      else
                        Dict.insert id sdl dl
                    , if isEmpty sdr then
                        dr

                      else
                        Dict.insert id sdr dr
                    )
                )
                (\id sm ( dl, dr ) -> ( dl, Dict.insert id sm dr ))
                da
                db
                ( Dict.empty, Dict.empty )
    in
    if ma == mb then
        ( PathMap Nothing dl_, PathMap Nothing dr_ )

    else
        ( PathMap ma dl_, PathMap mb dr_ )


joinMap : PathMap a -> (a -> PathMap b) -> PathMap b
joinMap (PathMap ma d) f =
    merge always
        (Maybe.unwrap empty f ma)
        (PathMap Nothing <| Dict.map (\_ p -> joinMap p f) d)


append : Paths -> Paths -> Paths
append =
    merge always


joinSub : Paths -> Paths -> Paths
joinSub p s =
    joinMap p <| always s


push : Path -> Paths -> Paths
push path =
    insert path ()
