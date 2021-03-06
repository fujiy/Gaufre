module Firestore.Path exposing (..)

import Dict.Extra as Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe


type Id a
    = Id String


type alias SomeId =
    String


type Path
    = Root
    | RootCol SomeId Col


type Col
    = Col
    | SubDoc SomeId Doc
    | Query String String Value Col


type Doc
    = Doc
    | SubCol SomeId Col


coerce : Id a -> Id b
coerce (Id str) =
    Id str


unId : Id a -> String
unId (Id str) =
    str


root : Path
root =
    Root


isRoot : Path -> Bool
isRoot =
    (==) Root


topLevel : SomeId -> Path
topLevel id =
    RootCol id Col


getLast : Path -> Maybe SomeId
getLast p =
    let
        goCol col =
            case col of
                Col ->
                    Nothing

                SubDoc id doc ->
                    Maybe.or (goDoc doc) (Just id)

                Query _ _ _ col_ ->
                    goCol col_

        goDoc doc =
            case doc of
                Doc ->
                    Nothing

                SubCol id col ->
                    Maybe.or (goCol col) (Just id)
    in
    case p of
        Root ->
            Nothing

        RootCol id col ->
            Maybe.or (goCol col) (Just id)


fromIds : List SomeId -> Path
fromIds xs =
    let
        goCol ids =
            case ids of
                id :: ids_ ->
                    SubDoc id <| goDoc ids_

                [] ->
                    Col

        goDoc ids =
            case ids of
                id :: ids_ ->
                    SubCol id <| goCol ids_

                [] ->
                    Doc
    in
    case xs of
        [] ->
            Root

        id :: ids ->
            RootCol id <| goCol ids


fromString : String -> Path
fromString str =
    case String.split "/" str of
        [ "" ] ->
            Root

        ids ->
            fromIds ids


encode : Path -> Value
encode path =
    case path of
        Root ->
            Encode.object <|
                [ ( "type", Encode.string "root" ) ]

        RootCol id col ->
            Encode.object <|
                [ ( "type", Encode.string "col" )
                , ( "id", Encode.string id )
                , ( "sub", encodeCol col )
                ]


encodeCol : Col -> Value
encodeCol col =
    case col of
        Col ->
            Encode.object <|
                [ ( "type", Encode.string "end" ) ]

        SubDoc id doc ->
            Encode.object <|
                [ ( "type", Encode.string "doc" )
                , ( "id", Encode.string id )
                , ( "sub", encodeDoc doc )
                ]

        Query field op value col_ ->
            Encode.object <|
                [ ( "type", Encode.string "query" )
                , ( "field", Encode.string field )
                , ( "op", Encode.string op )
                , ( "value", value )
                , ( "sub", encodeCol col_ )
                ]


encodeDoc : Doc -> Value
encodeDoc doc =
    case doc of
        Doc ->
            Encode.object <|
                [ ( "type", Encode.string "end" ) ]

        SubCol id col ->
            Encode.object <|
                [ ( "type", Encode.string "col" )
                , ( "id", Encode.string id )
                , ( "sub", encodeCol col )
                ]


decode : Decoder Path
decode =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "root" ->
                        Decode.succeed Root

                    "col" ->
                        Decode.map2 RootCol
                            (Decode.field "id" Decode.string)
                            (Decode.field "sub" decodeCol)

                    _ ->
                        Decode.fail "root path"
            )


decodeCol : Decoder Col
decodeCol =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "end" ->
                        Decode.succeed Col

                    "doc" ->
                        Decode.map2 SubDoc
                            (Decode.field "id" Decode.string)
                            (Decode.field "sub" decodeDoc)

                    "query" ->
                        Decode.map4 Query
                            (Decode.field "field" Decode.string)
                            (Decode.field "op" Decode.string)
                            (Decode.field "value" Decode.value)
                            (Decode.field "sub" <|
                                Decode.lazy <|
                                    \_ -> decodeCol
                            )

                    _ ->
                        Decode.fail "collection path"
            )


decodeDoc : Decoder Doc
decodeDoc =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "end" ->
                        Decode.succeed Doc

                    "col" ->
                        Decode.map2 SubCol
                            (Decode.field "id" Decode.string)
                            (Decode.field "sub" <|
                                Decode.lazy <|
                                    \_ -> decodeCol
                            )

                    _ ->
                        Decode.fail "document path"
            )
