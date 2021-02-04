module Data exposing (..)

import Array
import Browser.Navigation as Nav
import Data.Client as Client exposing (Client)
import Data.Project as Project exposing (Project)
import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Decode as Decode
import Firestore.Encode as Encode
import Firestore.Update as Update
import Maybe.Extra as Maybe
import Monocle.Lens exposing (Lens)


type alias Auth =
    { uid : String
    , name : String
    , token : String
    }


type alias Data =
    { users : Collection User
    , projects : Collection Project
    }



-- clientsLens : Lens Data (Collection Client)
-- clientsLens = Lens .clients (\b a -> { a | clients = b })


usersLens : Lens Data (Collection User)
usersLens =
    Lens .users (\b a -> { a | users = b })


projectsLens : Lens Data (Collection Project)
projectsLens =
    Lens .projects (\b a -> { a | projects = b })


encode : Encode.Encoder (Firestore Data)
encode =
    Encode.firestore
        [ Encode.collection "users" .users User.encode
        , Encode.collection "projects" .projects Project.encode
        ]


decode : Decode.Decoder (Firestore Data)
decode =
    Decode.firestore Data
        |> Decode.collection "users" User.decode
        |> Decode.collection "projects" Project.decode


initClient : Auth -> Update.Updater (Firestore Data)
initClient auth =
    Update.firestore <|
        Update.updates
            [ Update.collection usersLens <|
                Update.doc auth.uid <|
                    Update.default User.encode
                        { name = auth.name, projects = Array.empty }
            ]
