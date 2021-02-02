module Firestore.Encode exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Firestore.Internal as Internal exposing (..)
import Json.Encode as Json
import Maybe.Extra as Maybe
import Util exposing (..)


type alias Encoder a =
    a -> Value a


firestore : List ( Id, Encoder r ) -> Encoder (Firestore r)
firestore fields (Firestore r) =
    Value <|
        Json.object <|
            List.map (\( k, enc ) -> ( k, unValue <| enc r.data )) fields


subCollection :
    String
    -> (r -> Collection a)
    -> Encoder (Document a)
    -> ( String, Encoder r )
subCollection =
    collection


collection :
    Id
    -> (r -> Collection a)
    -> Encoder (Document a)
    -> ( Id, Encoder r )
collection name getter enc =
    ( name, getter >> collection_ enc >> coerce )


collection_ : Encoder (Document r) -> Encoder (Collection r)
collection_ f (Collection c) =
    Value <|
        Json.object
            [ ( "path", unValue <| path c.path )
            , ( "documents", Json.dict identity (f >> unValue) c.documents )
            ]


document : List ( String, Encoder r ) -> Encoder (Document r)
document fields (Document d) =
    let
        ( status, v ) =
            case d.data of
                Loading ->
                    ( "loading", [] )

                Failure ->
                    ( "failure", [] )

                Committing r ->
                    ( "committing"
                    , List.map (\( k, enc ) -> ( k, unValue <| enc r )) fields
                    )

                UpToDate r ->
                    ( "uptodate"
                    , List.map (\( k, enc ) -> ( k, unValue <| enc r )) fields
                    )
    in
    Value <|
        Json.object
            [ ( "path", unValue <| path d.path )
            , ( "status", Json.string status )
            , ( "__doc__", Json.bool True )
            , ( "data", Json.object v )
            ]


mapDocument : (a -> r) -> Encoder (Document r) -> Encoder (Document a)
mapDocument f enc a =
    coerce <| enc <| Internal.mapDocument f a


field : String -> (r -> a) -> Encoder a -> ( String, Encoder r )
field name getter enc =
    ( name, getter >> enc >> coerce )


reference : Encoder (Document r) -> Encoder (Reference r)
reference f (Reference d) =
    f d |> coerce


path : Encoder Path
path =
    Value << Json.array Json.string


string : Encoder String
string =
    Value << Json.string


bool : Encoder Bool
bool =
    Value << Json.bool


int : Encoder Int
int =
    Value << Json.int


float : Encoder Float
float =
    Value << Json.float


list : Encoder a -> Encoder (List a)
list f =
    Value << Json.list (unValue << f)


array : Encoder a -> Encoder (Array a)
array f =
    Value << Json.array (unValue << f)


dict : Encoder a -> Encoder (Dict String a)
dict f =
    Value << Json.dict identity (unValue << f)


lazy : (() -> Encoder a) -> Encoder a
lazy f a =
    f () a


map : (b -> a) -> Encoder a -> Encoder b
map f enc b =
    coerce <| enc <| f b


null : Value ()
null =
    Value Json.null


coerce : Value a -> Value b
coerce (Value v) =
    Value v


updates : Encoder a -> Encoder (Updates a)
updates enc { value, documents, collections, requests } =
    Json.object
        [ ( "value", unValue <| enc value )
        , ( "documents"
          , flip Json.array documents <|
                \( p, du ) ->
                    case du of
                        Whole (Value v) ->
                            Json.object
                                [ ( "path", unValue <| path p )
                                , ( "type", Json.string "whole" )
                                , ( "value", v )
                                ]

                        Field name (Value v) ->
                            Json.object
                                [ ( "path", unValue <| path p )
                                , ( "type", Json.string "field" )
                                , ( "name", Json.string "name" )
                                , ( "value", v )
                                ]

                        Delete ->
                            Json.object
                                [ ( "path", unValue <| path p )
                                , ( "type", Json.string "delete" )
                                ]
          )
        , ( "requests", Json.array (path >> unValue) requests )
        ]
        |> Value
