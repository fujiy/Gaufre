module Data exposing (..)

import Array
import Browser.Navigation as Nav
import Data.Client as Client
import Data.Project as Project exposing (Project)
import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Desc as Desc exposing (FirestoreDesc)
import Firestore.Lens as Lens exposing (o, where_)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, unId)
import Firestore.Update as Update exposing (Updater)
import GDrive
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



-- Utilities


userRole : Project -> Id User -> Project.Role
userRole p id =
    if Firestore.getId p.owner == id then
        Project.Owner

    else if List.any (Firestore.getId >> (==) id) p.admins then
        Project.Admin

    else
        Project.Staff


myRole : Project -> Auth -> Project.Role
myRole p auth =
    userRole p <| Id.fromString auth.uid



-- Desc


desc : FirestoreDesc Data
desc =
    Desc.collection "users" .users User.desc
        >> Desc.collection "clients" .clients Client.desc
        >> Desc.collection "projects" .projects Project.desc
        |> Desc.firestore Data



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


myId : Auth -> Id x
myId auth =
    Id.fromString auth.uid


myClient : Auth -> Lens Root Data Doc Client.Document
myClient auth =
    o clients <| Lens.doc <| myId auth


me : Auth -> Lens Root Data Doc User.Document
me auth =
    o users <| Lens.doc <| myId auth


userHasEmail : String -> Lens Root Data Col User.Collection
userHasEmail email =
    o users <| Lens.where_ "email" Lens.EQ Desc.string email


userRef : Id User -> User.Reference
userRef id =
    Firestore.ref <| Path.fromIds [ "users", Id.unId id ]


myRef : Auth -> User.Reference
myRef auth =
    userRef <| myId auth



-- Project


myProjects : Auth -> Lens Root Data Col Project.Collection
myProjects auth =
    o projects <|
        Lens.where_ "members" Lens.CONTAINS Desc.reference (myRef auth)


currentProject : Auth -> Int -> Lens Root Data Doc Project.Document
currentProject auth i =
    projectDeref <|
        o (myClient auth) <|
            o Lens.get <|
                o Client.projects <|
                    Lens.atArray i


project : Id Project -> Lens Root Data Doc Project.Document
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


projectRef : Id Project -> Project.Reference
projectRef id =
    Firestore.ref <| Path.fromIds [ "projects", unId id ]



-- Updaters


initClient : Auth -> User -> Updater Data msg
initClient auth user =
    Update.all
        [ Update.default (me auth) User.desc user
        , Update.default (myClient auth) Client.desc <|
            { projects = Array.empty }
        ]


inviteMember : Auth -> Id Project -> String -> Updater Data (Maybe User)
inviteMember auth projectId name =
    let
        email =
            name ++ "@gmail.com"
    in
    Update.andThen
        (Access.access <|
            (o users <|
                o (where_ "email" Lens.EQ Desc.string email) Lens.getAll
            )
        )
        (\us ->
            case us of
                [ user ] ->
                    Update.all
                        [ Update.modify (project projectId) Project.desc <|
                            \p ->
                                { p
                                    | members =
                                        userRef (Id.self user) :: p.members
                                }
                        , Update.command <|
                            \_ ->
                                GDrive.permissions_create auth.token
                                    (unId projectId)
                                    { role = GDrive.Writer
                                    , type_ = GDrive.User user.email
                                    }
                                    |> Cmd.map (\_ -> Just user)
                        ]

                _ ->
                    Update.succeed Nothing
        )
