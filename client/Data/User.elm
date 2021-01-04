module Data.User exposing (..)

import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode


type alias User =
    { name : String
    }


encode : Encode.Encoder (Document User)
encode =
    Encode.document
        [ Encode.field "name" .name Encode.string
        ]


decode : Decode.Decoder (Document User)
decode =
    Decode.document User
        |> Decode.field "name" Decode.string
