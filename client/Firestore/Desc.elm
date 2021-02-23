module Firestore.Desc exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Firestore.Internal exposing (..)
import Firestore.Path as Path exposing (Path, PathMap, Paths)
import Firestore.Remote exposing (Remote(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe
import Monocle.Iso exposing (Iso)
import Util exposing (..)


type alias Iso a b =
    Monocle.Iso.Iso a b


type alias Encoder a =
    a -> Value


type alias Applier a =
    Applier_ a a


type alias Applier_ l r =
    PathMap Value -> r -> Result Decode.Error l


type alias Desc a =
    { encoder : Encoder a
    , decoder : Decoder a
    }


type DocumentDesc s r
    = DocumentDesc
        { encoder : Encoder (Remote r)
        , decoder : Decoder (Remote r)
        , applier : Applier s
        , empty : s
        }


type FirestoreDesc r
    = FirestoreDesc
        { applier : Applier r
        , empty : r
        }


type CollectionDesc l r
    = CollectionDesc
        { applier : Applier_ l r
        , empty : l
        }


unField (Field m) =
    m


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
    FirestoreDesc c


document :
    c
    -> (Field c r -> Field r r)
    -> DocumentDesc () r
document constr f =
    documentWithSubs constr f () identity


documentWithSubs :
    rc
    -> (Field rc r -> Field r r)
    -> sc
    -> (CollectionDesc sc s -> CollectionDesc s s)
    -> DocumentDesc s r
documentWithSubs rconstr rf sconstr sf =
    let
        (CollectionDesc cd) =
            sf <|
                CollectionDesc <|
                    { applier = \_ _ -> Ok sconstr
                    , empty = sconstr
                    }

        (Field fd) =
            rf <|
                Field
                    { encoders = []
                    , decoder = Decode.succeed rconstr
                    }

        desc =
            remote <|
                { encoder =
                    \r ->
                        Encode.object <|
                            List.map (\( name, f ) -> ( name, f r )) fd.encoders
                , decoder = fd.decoder
                }
    in
    DocumentDesc
        { encoder = desc.encoder
        , decoder = desc.decoder
        , applier =
            \pv ->
                if Path.isEmpty pv then
                    Ok

                else
                    cd.applier pv
        , empty = cd.empty
        }


collection :
    String
    -> (r -> Collection s a)
    -> DocumentDesc s a
    -> CollectionDesc (Collection s a -> l) r
    -> CollectionDesc l r
collection name getter (DocumentDesc d) (CollectionDesc c) =
    CollectionDesc
        { applier =
            \pv r ->
                Result.map2 identity
                    (c.applier pv r)
                    (let
                        (Collection col) =
                            getter r

                        upds =
                            Path.subMaps <| Path.at name pv
                     in
                     List.foldr
                        (\( id, dpv ) rd ->
                            let
                                (Document sub doc) =
                                    Dict.get id col.docs
                                        |> Maybe.withDefault
                                            (Document d.empty Loading)

                                sub_ =
                                    d.applier dpv sub

                                doc_ =
                                    Path.getRootItem dpv
                                        |> Maybe.unwrap (Ok doc)
                                            (Decode.decodeValue d.decoder)
                            in
                            Result.map2
                                (Dict.insert id)
                                (Result.map2 Document sub_ doc_)
                                rd
                        )
                        (Ok col.docs)
                        upds
                        |> Result.map
                            (\dic -> Collection { col | docs = dic })
                    )
        , empty =
            c.empty <|
                Collection { name = name, empty = d.empty, docs = Dict.empty }
        }


reference : Desc (Reference s r)
reference =
    Desc
        (\(Reference p) ->
            Encode.object [ ( "__path__", path.encoder p ) ]
        )
        (Decode.field "path" Decode.string
            |> Decode.map (String.split "/" >> Path.fromList >> Reference)
        )


remote : Desc a -> Desc (Remote a)
remote d =
    { encoder =
        \ra ->
            let
                ( status, v ) =
                    case ra of
                        Loading ->
                            ( "loading", Encode.null )

                        Failure ->
                            ( "failure", Encode.null )

                        Committing a ->
                            ( "committing", d.encoder a )

                        UpToDate a ->
                            ( "uptodate", d.encoder a )
            in
            Encode.object
                [ ( "status", Encode.string status )
                , ( "value", v )
                ]
    , decoder =
        Decode.andThen
            (\t ->
                case t of
                    ( "loading", _ ) ->
                        Decode.succeed Loading

                    ( "failure", _ ) ->
                        Decode.succeed Failure

                    ( "committing", Nothing ) ->
                        Decode.fail "no value"

                    ( "committing", Just a ) ->
                        Decode.succeed <| Committing a

                    ( "uptodate", Nothing ) ->
                        Decode.fail "no value"

                    ( "uptodate", Just a ) ->
                        Decode.succeed <| UpToDate a

                    _ ->
                        Decode.fail "unknown state"
            )
        <|
            Decode.map2 Tuple.pair
                (Decode.field "status" Decode.string)
                (Decode.field "value" <| Decode.nullable d.decoder)
    }


pathMap : Desc a -> Desc (PathMap a)
pathMap d =
    object Path.PathMap <|
        field "item" Path.getRootItem (nullable d)
            >> field "sub" Path.subMaps_ (dict <| lazy <| \_ -> pathMap d)


paths : Desc Paths
paths =
    pathMap void


path : Desc Path
path =
    array string



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
