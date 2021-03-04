module Data exposing (..)

import Array
import Browser.Navigation as Nav
import Data.Client as Client
import Data.Project as Project exposing (Project)
import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (FirestoreDesc)
import Firestore.Lens as Lens exposing (QueryOp(..), o)
import Firestore.Path as Path exposing (Id)
import Firestore.Update as Update exposing (Updater)
import Maybe.Extra as Maybe


type alias Auth =
    { uid : String
    , token : String
    , navKey : Nav.Key
    }


type alias Data =
    { users : User.Collection
    , clients : Client.Collection
    , projects : Project.Collection
    }



-- Lenses
-- Collection


clients : Lens Root Data Col Client.Collection
clients =
    Lens.collection .clients (\b a -> { a | clients = b })


users : Lens Root Data Col User.Collection
users =
    Lens.collection .users (\b a -> { a | users = b })


projects : Lens Root Data Col Project.Collection
projects =
    Lens.collection .projects (\b a -> { a | projects = b })



-- User


myClient : Auth -> Lens Root Data Doc Client.Document
myClient auth =
    o clients <| Lens.doc auth.uid


me : Auth -> Lens Root Data Doc User.Document
me auth =
    o users <| Lens.doc auth.uid


userRef : Id -> User.Reference
userRef id =
    Firestore.ref <| Path.fromIds [ "users", id ]



-- Project


myProjects : Auth -> Lens Root Data Col Project.Collection
myProjects auth =
    o projects <|
        Lens.where_ "members" CONTAINS Desc.reference (userRef auth.uid)



-- myProjects : Auth -> Lens Root Data Doc (List Project.Document)
-- myProjects auth =
--     Lens.derefs projects Lens.end <|
--         o (myClient auth) <|
--             o Lens.get <|
--                 o Client.projects <|
--                     Lens.fromIso (Lens.reverse Lens.list2array)


currentProject : Auth -> Int -> Lens Root Data Doc Project.Document
currentProject auth i =
    projectDeref <|
        o (myClient auth) <|
            o Lens.get <|
                o Client.projects <|
                    Lens.atArray i


project : Id -> Lens Root Data Doc Project.Document
project id =
    o projects <| Lens.doc id


projectDeref :
    Lens Root Data Item Project.Reference
    -> Lens Root Data Doc Project.Document
projectDeref =
    Lens.deref projects Lens.end


projectMembers : Project -> Lens Root Data Doc (List User.Document)
projectMembers p =
    Lens.derefs users Lens.end <|
        Lens.const p.members


desc : FirestoreDesc Data
desc =
    Desc.collection "users" .users User.desc
        >> Desc.collection "clients" .clients Client.desc
        >> Desc.collection "projects" .projects Project.desc
        |> Desc.firestore Data


initClient : Auth -> User -> Updater Data
initClient auth user =
    Update.all
        [ Update.default (me auth) User.desc user
        , Update.default (myClient auth) Client.desc <|
            { projects = Array.empty }
        ]
