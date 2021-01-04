module Data exposing (..)

import Data.Client as Client exposing (Client)
import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode
import Firestore.Update as Update
import Maybe.Extra as Maybe


type alias Auth =
    { uid : String
    , name : String
    , token : String
    }


type alias Data =
    { clients : Collection Client
    , users : Collection User
    }


encode : Encode.Encoder (Firestore Data)
encode =
    Encode.firestore
        [ Encode.collection "clients" .clients Client.encode
        , Encode.collection "users" .users User.encode
        ]


decode : Decode.Decoder (Firestore Data)
decode =
    Decode.firestore Data
        |> Decode.collection "clients" Client.decode
        |> Decode.collection "users" User.decode


initClient : Auth -> Update.Updater (Firestore Data)
initClient auth =
    Update.firestore <|
        Update.collection .users (\r u -> { r | users = u }) <|
            Update.doc auth.uid <|
                Update.default User.encode { name = auth.name }
