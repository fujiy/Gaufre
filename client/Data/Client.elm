module Data.Client exposing (..)

import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode


type alias Client =
    { user : Reference User }


encode : Encode.Encoder (Document Client)
encode =
    Encode.document
        [ Encode.field "user" .user <| Encode.reference User.encode
        ]


decode : Decode.Decoder (Document Client)
decode =
    Decode.document Client
        |> Decode.field "user" (Decode.reference User.decode)
