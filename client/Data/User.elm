module Data.User exposing (..)

import Array exposing (Array)
import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode
import GDrive


type alias User =
    { name : String
    , projects : Array (Reference Project)
    }


type Project
    = Project Project_


type alias Project_ =
    { name : String
    , members : List (Reference User)
    }


encode : Encode.Encoder (Document User)
encode =
    Encode.document
        [ Encode.field "name" .name Encode.string
        , Encode.field "projects" .projects <|
            Encode.array <|
                Encode.reference encodeProject
        ]


decode : Decode.Decoder (Document User)
decode =
    Decode.document User
        |> Decode.field "name" Decode.string
        |> Decode.field "projects"
            (Decode.array <| Decode.reference decodeProject)


encodeProject : Encode.Encoder (Document Project)
encodeProject =
    Encode.document
        [ Encode.field "name" .name Encode.string
        , Encode.field "members" .members <|
            Encode.list <|
                Encode.reference <|
                    Encode.lazy <|
                        \_ -> encode
        ]
        |> Encode.mapDocument (\(Project p) -> p)


decodeProject : Decode.Decoder (Document Project)
decodeProject =
    Decode.document Project_
        |> Decode.field "name" Decode.string
        |> Decode.field "members"
            (Decode.list <| Decode.reference <| Decode.lazy <| \_ -> decode)
        |> Decode.mapDocument Project
