module Firestore.Desc exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Firestore.Internal exposing (..)
import Firestore.Path as Path exposing (Path)
import Firestore.Path.Id as Id exposing (Id(..), SelfId)
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Map as PathMap exposing (Paths)
import Firestore.Remote as Remote exposing (Remote(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Iso exposing (Iso)
import Time
import Util exposing (..)


type alias Iso a b =
    Monocle.Iso.Iso a b


type alias Encoder a =
    a -> Value


type alias Applier p a =
    Applier_ p a a


type alias Applier_ p l r =
    p -> r -> Result Decode.Error l


type Update
    = Got Value
    | Clear


type alias Desc a =
    { encoder : Encoder a
    , decoder : Decoder a
    }


type DocumentDesc s r
    = DocumentDesc
        { encoder : Encoder (Remote r)
        , decoder : Decoder (Remote r)
        , applier : Applier (PathMap.Doc Update) s
        , empty : s
        }


type FirestoreDesc r
    = FirestoreDesc
        { applier : Applier (PathMap.Map Update) r
        , empty : r
        }


type CollectionDesc l r
    = CollectionDesc
        { applier : Applier_ (IdMap.Map r (PathMap.Col Update)) l r
        , empty : l
        }


unField (Field m) =
    m


iso : (a -> b) -> (b -> a) -> Iso a b
iso =
    Iso


map : Iso a b -> Desc a -> Desc b
map { get, reverseGet } d =
    { encoder = reverseGet >> d.encoder
    , decoder = Decode.map get d.decoder
    }


firestore :
    c
    -> (CollectionDesc c r -> CollectionDesc r r)
    -> FirestoreDesc r
firestore constr rf =
    let
        (CollectionDesc c) =
            rf <|
                CollectionDesc <|
                    { applier = \_ _ -> Ok constr
                    , empty = constr
                    }
    in
    FirestoreDesc
        { applier = \pvs cols -> c.applier (PathMap.cols pvs) cols
        , empty = c.empty
        }


document : c -> (Field c r -> Field r r) -> DocumentDesc () r
document constr =
    documentWithId <| \_ -> constr


documentWithId :
    (SelfId -> c)
    -> (Field c r -> Field r r)
    -> DocumentDesc () r
documentWithId constr f =
    documentWithIdAndSubs constr f () identity


documentWithSubs :
    rc
    -> (Field rc r -> Field r r)
    -> sc
    -> (CollectionDesc sc s -> CollectionDesc s s)
    -> DocumentDesc s r
documentWithSubs constr =
    documentWithIdAndSubs <| \_ -> constr


documentWithIdAndSubs :
    (SelfId -> rc)
    -> (Field rc r -> Field r r)
    -> sc
    -> (CollectionDesc sc s -> CollectionDesc s s)
    -> DocumentDesc s r
documentWithIdAndSubs rconstr rf sconstr sf =
    let
        (CollectionDesc cd) =
            sf <|
                CollectionDesc <|
                    { applier = \_ _ -> Ok sconstr
                    , empty = sconstr
                    }

        fld did =
            unField <|
                rf <|
                    Field
                        { encoders = []
                        , decoder = Decode.succeed <| rconstr did
                        }
    in
    DocumentDesc
        { encoder =
            Remote.encode <|
                \r ->
                    Encode.object <|
                        List.map (\( name, f ) -> ( name, f r ))
                            (fld "").encoders
        , decoder =
            Decode.andThen
                (\did -> Remote.decode (fld did).decoder)
                (Decode.field "id" Decode.string)
        , applier =
            \pdv ->
                if PathMap.isEmptyDoc pdv then
                    Ok

                else
                    cd.applier <| PathMap.subCols pdv
        , empty = cd.empty
        }


collection :
    String
    -> (r -> Collection s a)
    -> DocumentDesc s a
    -> CollectionDesc (Collection s a -> l) r
    -> CollectionDesc l r
collection name getter (DocumentDesc d) (CollectionDesc c) =
    let
        empty =
            c.empty <|
                Collection
                    { name = Id name
                    , empty = d.empty
                    , loading = True
                    , docs = IdMap.empty
                    , q = Dict.empty
                    }

        applier_ cpvs r =
            Result.map2 identity
                (c.applier cpvs r)
                (applier cpvs <| getter r)

        applier cpvs (Collection col) =
            let
                cpv =
                    IdMap.get (Id name) cpvs
                        |> Maybe.withDefault PathMap.emptyCol
            in
            if PathMap.isEmptyCol cpv then
                Ok <| Collection col

            else
                case PathMap.getColRoot cpv of
                    Just Clear ->
                        Ok <|
                            Collection
                                { col
                                    | loading = True
                                    , docs = IdMap.empty
                                    , q = Dict.empty
                                }

                    _ ->
                        Result.map2
                            (\docs q ->
                                Collection
                                    { col
                                        | docs = docs
                                        , q = q
                                        , loading = False
                                    }
                            )
                            (List.foldr
                                (\( did, dpv ) rd ->
                                    let
                                        (Document sub doc) =
                                            IdMap.get did col.docs
                                                |> Maybe.withDefault
                                                    (Document d.empty Loading)

                                        sub_ =
                                            d.applier dpv sub

                                        doc_ =
                                            case PathMap.getDocRoot dpv of
                                                Nothing ->
                                                    Ok doc

                                                Just Clear ->
                                                    Ok Loading

                                                Just (Got v) ->
                                                    Decode.decodeValue
                                                        d.decoder
                                                        v
                                    in
                                    Result.map2
                                        (IdMap.insert did)
                                        (Result.map2 Document sub_ doc_)
                                        rd
                                )
                                (Ok col.docs)
                                (PathMap.subDocs cpv |> IdMap.toList)
                            )
                            (List.foldr
                                (\qpv rq ->
                                    let
                                        key =
                                            queryKey qpv.field qpv.op qpv.value

                                        qcol =
                                            Dict.get key col.q
                                                |> Maybe.withDefault
                                                    (Collection
                                                        { name = Id name
                                                        , empty = d.empty
                                                        , loading = True
                                                        , docs = IdMap.empty
                                                        , q = Dict.empty
                                                        }
                                                    )

                                        qcol_ =
                                            applier
                                                (IdMap.singleton (Id name)
                                                    qpv.col
                                                )
                                                qcol
                                    in
                                    Result.map2 (Dict.insert key) qcol_ rq
                                )
                                (Ok col.q)
                                (PathMap.subQueries cpv)
                            )
    in
    CollectionDesc { applier = applier_, empty = empty }


reference : Desc (Reference s r)
reference =
    Desc
        (\(Reference p) ->
            Encode.object [ ( "__path", path.encoder p ) ]
        )
        (Decode.field "path" Decode.string
            |> Decode.map (Path.fromString >> Reference)
        )


references : Desc (List (Reference s r))
references =
    list reference


timestamp : Desc Timestamp
timestamp =
    Desc
        (\t ->
            case t of
                Timestamp s n ->
                    Encode.object
                        [ ( "__timestamp", Encode.bool True )
                        , ( "seconds", Encode.int s )
                        , ( "nanoseconds", Encode.int n )
                        ]

                ServerTimestamp ->
                    Encode.object
                        [ ( "__timestamp", Encode.string "server" ) ]
        )
        (Decode.map2 Timestamp
            (Decode.field "seconds" Decode.int)
            (Decode.field "nanoseconds" Decode.int)
        )


remote : Desc a -> Desc (Remote a)
remote d =
    Desc (Remote.encode d.encoder) (Remote.decode d.decoder)


id : Desc (Id a)
id =
    map (Iso Id (\(Id s) -> s)) string


ids : Desc (List (Id a))
ids =
    list id


pathMap : Desc a -> Desc (PathMap.Map a)
pathMap desc =
    Desc (PathMap.encode desc.encoder) (PathMap.decode desc.decoder)


paths : Desc Paths
paths =
    pathMap void


path : Desc Path
path =
    Desc Path.encode Path.decode


request : Desc Request
request =
    Desc
        (\req ->
            if req.set then
                case req.value of
                    Just v ->
                        Encode.object
                            [ ( "type", Encode.string "set" ), ( "value", v ) ]

                    Nothing ->
                        Encode.object [ ( "type", Encode.string "delete" ) ]

            else
                Encode.null
        )
        (Decode.oneOf
            [ Decode.andThen
                (\t ->
                    case t of
                        "get" ->
                            Decode.succeed <| Request True False Nothing

                        "set" ->
                            Decode.map (Request False True << Just) <|
                                Decode.field "value" Decode.value

                        "add" ->
                            Decode.map (Request False True << Just) <|
                                Decode.field "value" Decode.value

                        "delete" ->
                            Decode.succeed <| Request False True Nothing

                        _ ->
                            Decode.fail <| "unknown type: " ++ t
                )
                (Decode.field "type" Decode.string)
            , Decode.succeed <| Request False False Nothing
            ]
        )


idMap : Desc a -> Desc (IdMap.Map x a)
idMap desc =
    map (Iso IdMap.fromDict IdMap.toDict) (dict desc)


update : Desc Update
update =
    Desc
        (\u ->
            case u of
                Clear ->
                    Encode.null

                Got v ->
                    v
        )
        (Decode.map Got Decode.value)



-- Primitives


string : Desc String
string =
    Desc Encode.string Decode.string


int : Desc Int
int =
    Desc Encode.int Decode.int


float : Desc Float
float =
    Desc Encode.float Decode.float


bool : Desc Bool
bool =
    Desc Encode.bool Decode.bool


null : a -> Desc a
null a =
    Desc (\_ -> Encode.null) (Decode.null a)


void : Desc ()
void =
    Desc (\_ -> Encode.object [])
        (Decode.dict Decode.value |> Decode.map (always ()))


value : Desc Value
value =
    Desc identity Decode.value


array : Desc a -> Desc (Array a)
array d =
    Desc (Encode.array d.encoder) (Decode.array d.decoder)


list : Desc a -> Desc (List a)
list d =
    Desc (Encode.list d.encoder) (Decode.list d.decoder)


dict : Desc a -> Desc (Dict String a)
dict d =
    Desc (Encode.dict identity d.encoder) (Decode.dict d.decoder)


nullable : Desc a -> Desc (Maybe a)
nullable d =
    Desc (Maybe.unwrap Encode.null d.encoder) (Decode.nullable d.decoder)


lazy : (() -> Desc a) -> Desc a
lazy df =
    Desc (\a -> (df ()).encoder a) (Decode.lazy (\_ -> (df ()).decoder))


enum : List ( String, a ) -> Desc a
enum xs =
    Desc
        (\a ->
            List.find (\( _, a_ ) -> a == a_) xs
                |> Maybe.unwrap Encode.null (\( s, _ ) -> Encode.string s)
        )
        (Decode.string
            |> Decode.andThen
                (\s ->
                    List.find (\( s_, _ ) -> s == s_) xs
                        |> Maybe.unwrap
                            (Decode.fail "no matching enum member")
                            (\( _, a ) -> Decode.succeed a)
                )
        )



-- Objects


type Field l r
    = Field
        { encoders : List ( String, Encoder r )
        , decoder : Decoder l
        }


object : c -> (Field c r -> Field r r) -> Desc r
object constr rf =
    let
        (Field { encoders, decoder }) =
            rf <|
                Field
                    { encoders = []
                    , decoder = Decode.succeed constr
                    }
    in
    { encoder =
        \r ->
            Encode.object <|
                List.map (\( name, f ) -> ( name, f r )) encoders
    , decoder = decoder
    }


field : String -> (r -> a) -> Desc a -> Field (a -> l) r -> Field l r
field name getter d (Field m) =
    Field
        { encoders = ( name, getter >> d.encoder ) :: m.encoders
        , decoder =
            Decode.map2 identity m.decoder (Decode.field name d.decoder)
        }


maybe :
    String
    -> (r -> Maybe a)
    -> Desc a
    -> Field (Maybe a -> l) r
    -> Field l r
maybe name getter d (Field m) =
    Field
        { encoders =
            ( name, getter >> Maybe.unwrap Encode.null d.encoder )
                :: m.encoders
        , decoder =
            Decode.map2 identity
                m.decoder
                (Decode.maybe <| Decode.field name d.decoder)
        }


optional :
    String
    -> (r -> a)
    -> a
    -> Desc a
    -> Field (a -> l) r
    -> Field l r
optional name getter default d (Field m) =
    Field
        { encoders =
            ( name, getter >> d.encoder )
                :: m.encoders
        , decoder =
            Decode.map2 identity
                m.decoder
                (Decode.map (Maybe.withDefault default) <|
                    Decode.maybe <|
                        Decode.field name d.decoder
                )
        }
