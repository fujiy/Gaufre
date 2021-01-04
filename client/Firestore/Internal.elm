module Firestore.Internal exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as JE
import List


type alias Id =
    String


type alias Path =
    Array Id


type Remote a
    = Loading
    | Failure
    | Committing a
    | UpToDate a


mapRemote : (a -> b) -> Remote a -> Remote b
mapRemote f r =
    case r of
        Loading ->
            Loading

        Failure ->
            Failure

        Committing a ->
            Committing (f a)

        UpToDate a ->
            UpToDate (f a)


appRemote : Remote (a -> b) -> Remote a -> Remote b
appRemote rf ra =
    case ( rf, ra ) of
        ( Failure, _ ) ->
            Failure

        ( _, Failure ) ->
            Failure

        ( Loading, _ ) ->
            Loading

        ( _, Loading ) ->
            Loading

        ( Committing f, Committing a ) ->
            Committing (f a)

        ( Committing f, UpToDate a ) ->
            Committing (f a)

        ( UpToDate f, Committing a ) ->
            Committing (f a)

        ( UpToDate f, UpToDate a ) ->
            UpToDate (f a)


type alias IsLoading =
    Bool


type Firestore r
    = Firestore
        { data : r
        , laters : Updater (Firestore r)
        }


type Collection r
    = Collection
        { path : Path
        , documents : Dict Id (Document r)
        }


type Document r
    = Document
        { path : Path
        , data : Remote r
        }


type Reference r
    = Reference Path (Document r)


type Value a
    = Value JE.Value


type Updater a
    = Updater (a -> Updates a)


type alias Updates a =
    { value : a
    , documents : Array ( Path, DocumentUpdates )
    , collections : Array ( Path, CollectionUpdates )
    , laters : Updater a
    }


type DocumentUpdates
    = Whole (Value ())
    | Field String (Value ())
    | Delete


type CollectionUpdates
    = Add (Value ())


noUpdates : a -> Updates a
noUpdates a =
    Updates a Array.empty Array.empty noUpdater


runUpdater : Updater a -> a -> Updates a
runUpdater (Updater f) =
    f


noUpdater : Updater a
noUpdater =
    Updater noUpdates


topLevel : Id -> Path
topLevel id =
    Array.fromList [ id ]


fromJson : JE.Value -> Value a
fromJson =
    Value


unValue : Value a -> JE.Value
unValue (Value v) =
    v


sub : Path -> Id -> Path
sub path id =
    Array.push id path


loadingDocument : Path -> Document r
loadingDocument path =
    Document { path = path, data = Loading }


noDocument : Path -> Document r
noDocument path =
    Document { path = path, data = Failure }


type Refs
    = Root
    | Sub (Dict Id Refs)


emptyRefs : Refs
emptyRefs =
    Sub Dict.empty


subRef : Id -> Refs -> Refs
subRef id refs =
    Sub <| Dict.singleton id refs


mergeRefs : List Refs -> Refs
mergeRefs =
    let
        merge x y =
            case ( x, y ) of
                ( Root, _ ) ->
                    Root

                ( _, Root ) ->
                    Root

                ( Sub dx, Sub dy ) ->
                    Sub <|
                        Dict.merge
                            Dict.insert
                            (\id a b -> Dict.insert id <| merge a b)
                            Dict.insert
                            dx
                            dy
                            Dict.empty
    in
    List.foldr merge emptyRefs
