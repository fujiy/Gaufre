module Data.User exposing (..)

import Firestore exposing (Document)
import Firestore.Desc as Desc exposing (DocumentDesc)
import GDrive


type alias User =
    { name : String
    }


type alias Collection =
    Firestore.Collection () User


type alias Document =
    Firestore.Document () User


desc : DocumentDesc () User
desc =
    Desc.document User <|
        Desc.field "name" .name Desc.string



-- encode : Encode.Encoder (Document User)
-- encode =
--     Encode.document
--         [ Encode.field "name" .name Encode.string
--         ]
-- decode : Decode.Decoder (Document User)
-- decode =
--     Decode.document User
--         |> Decode.field "name" Decode.string
