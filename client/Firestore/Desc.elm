module Firestore.Desc exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Firestore.Internal exposing (..)
import Firestore.Path as Path exposing (Id, Path, PathMap, Paths)
import Firestore.Remote as Remote exposing (Remote(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Monocle.Iso
import Util exposing (..)


type alias Iso a b =
    Monocle.Iso.Iso a b


type alias Encoder a =
    a -> Value


type alias Applier a =
    Applier_ a a


type alias Applier_ l r =
    PathMap Value -> Path -> r -> Result Decode.Error l


type Desc a
    = Desc
        { encoder : Encoder a
        , decoder : Decoder a
        , applier : Applier a
        }


type DocumentDesc r
    = DocumentDesc
        { encoder : Encoder (Remote r)
        , decoder : Decoder (Remote r)
        , applier : Applier (Remote r)
        }


type FirestoreDesc r
    = FirestoreDesc (Applier r)


applyRoot : Decoder a -> Applier a
applyRoot decoder pv _ a =
    case Path.getRootItem pv of
        Nothing ->
            Ok a

        Just v ->
            Decode.decodeValue decoder v


applyNothing : Applier a
applyNothing _ _ a =
    Ok a


type Member l r
    = Member
        { encoders : List ( String, Encoder r )
        , decoder : Decoder l
        , applier : Applier_ l r
        }


type Field l r
    = Field (Member l r)


type RootCollection l r
    = RootCollection (Applier_ l r)


unField (Field m) =
    m


map : Iso a b -> Desc a -> Desc b
map { get, reverseGet } (Desc d) =
    Desc
        { encoder = reverseGet >> d.encoder
        , decoder = Decode.map get d.decoder
        , applier = \pv p a -> reverseGet a |> d.applier pv p |> Result.map get
        }


firestore :
    c
    -> (RootCollection c r -> RootCollection r r)
    -> FirestoreDesc r
firestore constr rf =
    let
        (RootCollection applier) =
            rf <| RootCollection <| \pv _ _ -> Ok constr
    in
    FirestoreDesc applier


collection :
    String
    -> (r -> Collection a)
    -> DocumentDesc a
    -> RootCollection (Collection a -> l) r
    -> RootCollection l r
collection name getter (DocumentDesc d) (RootCollection applier) =
    RootCollection <|
        \pv p r ->
            Result.map2 identity
                (applier pv p r)
                (let
                    (Collection _ dict) =
                        getter r

                    upds =
                        Path.subMaps <| Path.at name pv
                 in
                 List.foldr
                    (\( id, dpv ) rd ->
                        let
                            (Document doc) =
                                Dict.get id dict
                                    |> Maybe.withDefault (Document Loading)
                        in
                        Result.map2
                            (Dict.insert id)
                            (d.applier dpv (Path.sub p id) doc
                                |> Result.map Document
                            )
                            rd
                    )
                    (Ok dict)
                    upds
                    |> Result.map (Collection name)
                )


subCollection :
    String
    -> (r -> Collection a)
    -> DocumentDesc a
    -> Field (Collection a -> l) r
    -> Field l r
subCollection name getter d (Field (Member m)) =
    let
        (RootCollection applier) =
            collection name getter d <| RootCollection m.applier
    in
    Field <|
        Member
            { encoders = m.encoders
            , decoder =
                Decode.map2 identity m.decoder <|
                    Decode.succeed (Collection name Dict.empty)
            , applier = applier
            }


field : String -> (r -> a) -> Desc a -> Field (a -> l) r -> Field l r
field name getter (Desc d) (Field (Member m)) =
    Field <|
        Member
            { encoders = ( name, getter >> d.encoder ) :: m.encoders
            , decoder =
                Decode.map2 identity m.decoder (Decode.field name d.decoder)
            , applier =
                \pv p r ->
                    Result.map2 identity
                        (m.applier pv p r)
                        (d.applier (Path.at name pv) (Path.sub p name) <|
                            getter r
                        )
            }


document : c -> (Field c r -> Field r r) -> DocumentDesc r
document constr ff =
    let
        (Desc d) =
            remote <| record constr (Field >> ff >> unField)
    in
    DocumentDesc <|
        { d
            | applier =
                \pv p r ->
                    if Path.isEmpty pv then
                        Ok r

                    else
                        case Path.getRootItem pv of
                            Nothing ->
                                d.applier pv p r

                            Just v ->
                                Decode.decodeValue d.decoder v
        }


record : c -> (Member c r -> Member r r) -> Desc r
record constr mf =
    let
        (Member { encoders, decoder, applier }) =
            mf <|
                Member
                    { encoders = []
                    , decoder = Decode.succeed constr
                    , applier = \pv _ _ -> Ok constr
                    }
    in
    Desc
        { encoder =
            \r ->
                Encode.object <|
                    List.map (\( name, f ) -> ( name, f r )) encoders
        , decoder = decoder
        , applier = applier
        }


member : String -> (r -> a) -> Desc a -> Member (a -> l) r -> Member l r
member name getter (Desc d) (Member m) =
    Member
        { encoders = ( name, getter >> d.encoder ) :: m.encoders
        , decoder =
            Decode.map2 identity m.decoder (Decode.field name d.decoder)
        , applier =
            \pv p r ->
                Result.map2 identity
                    (m.applier pv p r)
                    (d.applier pv p <| getter r)
        }


reference : DocumentDesc r -> Desc (Reference r)
reference (DocumentDesc d) =
    Debug.todo ""


remote : Desc a -> Desc (Remote a)
remote (Desc d) =
    Desc
        { encoder =
            \ra ->
                let
                    ( status, value ) =
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
                    , ( "value", value )
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
        , applier = remoteApplier d.applier
        }


remoteApplier applier pv p ra =
    case ra of
        Failure ->
            Ok Failure

        Loading ->
            Ok Loading

        Committing a ->
            Result.map Committing <| applier pv p a

        UpToDate a ->
            Result.map UpToDate <| applier pv p a


type alias Hoge =
    { fst : Int, snd : String }


hoge : Desc Hoge
hoge =
    member "fst" .fst int
        >> member "snd" .snd string
        |> record Hoge


primitive : Encoder a -> Decoder a -> Desc a
primitive encoder decoder =
    Desc
        { encoder = encoder
        , decoder = decoder
        , applier = applyNothing
        }


string : Desc String
string =
    primitive Encode.string Decode.string


int : Desc Int
int =
    primitive Encode.int Decode.int


array : Desc a -> Desc (Array a)
array (Desc d) =
    Desc
        { encoder = Encode.array d.encoder
        , decoder = Decode.array d.decoder
        , applier =
            \pv p ->
                Array.foldl
                    (\a -> Result.map2 Array.push (d.applier pv p a))
                    (Ok Array.empty)
        }


list : Desc a -> Desc (List a)
list (Desc d) =
    Desc
        { encoder = Encode.list d.encoder
        , decoder = Decode.list d.decoder
        , applier =
            \pv p ->
                List.foldr
                    (\a -> Result.map2 (::) (d.applier pv p a))
                    (Ok [])
        }
