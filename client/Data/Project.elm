module Data.Project exposing (..)

import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode


type alias Project =
    { folder : String
    , name : String
    }


encode : Encode.Encoder (Document Project)
encode =
    Encode.document
        [ Encode.field "folder" .folder Encode.string
        , Encode.field "name" .name Encode.string
        ]


decode : Decode.Decoder (Document Project)
decode =
    Decode.document Project
        |> Decode.field "folder" Decode.string
        |> Decode.field "name" Decode.string
