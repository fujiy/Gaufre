module Data exposing (..)

import Array
import Data.Client as Client
import Data.Project as Project exposing (Project)
import Data.User as User
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (FirestoreDesc)
import Firestore.Lens as Lens exposing (o)
import Firestore.Update as Update
import Maybe.Extra as Maybe


type alias Auth =
    { uid : String
    , name : String
    , token : String
    }


type alias Data =
    { users : User.Collection
    , clients : Client.Collection
    , projects : Project.Collection
    }


clients : Lens Data Client.Collection
clients =
    Lens.collection .clients (\b a -> { a | clients = b })


users : Lens Data User.Collection
users =
    Lens.collection .users (\b a -> { a | users = b })


projects : Lens Data Project.Collection
projects =
    Lens.collection .projects (\b a -> { a | projects = b })


myClient : Auth -> Lens Data Client.Document
myClient auth =
    o clients <| Lens.doc auth.uid


me : Auth -> Lens Data User.Document
me auth =
    o users <| Lens.doc auth.uid


myProjects : Auth -> Lens Data (List Project.Document)
myProjects auth =
    Lens.derefs project <|
        o (myClient auth) <|
            o Lens.get <|
                Lens.composeIso Client.projects (Lens.reverse Lens.list2array)


currentProject : Auth -> Int -> Lens Data Project.Document
currentProject auth i =
    Lens.deref project <|
        o (myClient auth) <|
            o Lens.get <|
                o Client.projects <|
                    Lens.atArray i


project : Lens.Dereferer Data Project.Document
project =
    Lens.dereferer projects


desc : FirestoreDesc Data
desc =
    Desc.collection "users" .users User.desc
        >> Desc.collection "clients" .clients Client.desc
        >> Desc.collection "projects" .projects Project.desc
        |> Desc.firestore Data


initClient : Auth -> Update.Updater Data
initClient auth =
    Update.all
        [ Update.default (me auth) User.desc <|
            { name = auth.name }
        , Update.default (myClient auth) Client.desc <|
            { projects = Array.empty }
        ]
