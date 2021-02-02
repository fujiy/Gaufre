module Data.Project exposing (..)

import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode
import GDrive


type alias Project =
    User.Project


encode : Encode.Encoder (Document Project)
encode =
    User.encodeProject


decode : Decode.Decoder (Document Project)
decode =
    User.decodeProject
