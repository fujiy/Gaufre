module Data.Client exposing (..)

import Data.Project as Project exposing (Project)
import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode


type alias Client =
    { projects : List (Reference Project) }


encode : Encode.Encoder (Document Client)
encode =
    Encode.document
        [ Encode.field "projects" .projects <|
            Encode.list <|
                Encode.reference Project.encode
        ]


decode : Decode.Decoder (Document Client)
decode =
    Decode.document Client
        |> Decode.field "projects"
            (Decode.list <| Decode.reference Project.decode)
