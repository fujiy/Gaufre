module Data exposing (..)

import Array exposing (Array)
import Browser.Navigation as Nav
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (Desc, DocumentDesc, FirestoreDesc)
import Firestore.Lens as Lens exposing (o, where_)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, SelfId, unId)
import Firestore.Path.Id.Map as IdMap
import GDrive
import Maybe.Extra as Maybe



-- Data types


type alias Auth =
    { uid : String
    , token : String
    , navKey : Nav.Key
    }


type alias Data =
    { users : Collection () User
    , clients : Collection () Client
    , projects : Collection ProjectSub Project
    }


type alias User =
    { id : SelfId
    , name : String
    , image : String
    , email : String
    }


type alias Client =
    { projects : Array (Reference ProjectSub Project)
    }


type alias Project =
    { id : SelfId
    , name : String
    , members : List (Reference () User)
    , admins : List (Reference () User)
    , owner : Reference () User
    , processes : IdMap.Map Process Process
    , parts : IdMap.Map Part Part
    }


type alias ProjectSub =
    { works : Collection () Work
    }


type alias Work =
    { id : SelfId
    , name : String
    , process : Id Process
    , belongsTo : List (Id Part)
    , staffs : List (Reference () User)
    , reviewers : List (Reference () User)
    , waitingFor : List WorkRef
    }


type alias Process =
    { name : String
    , order : Float
    , upstreams : List SelfId
    }


type alias Part =
    { name : String
    , order : Float
    , parent : Maybe SelfId
    }


type WorkRef
    = Ref (Reference () Work)



-- Utilities
-- Desc


desc : FirestoreDesc Data
desc =
    Desc.collection "users" .users userDesc
        >> Desc.collection "clients" .clients clientDesc
        >> Desc.collection "projects" .projects projectDesc
        |> Desc.firestore Data


userDesc : DocumentDesc () User
userDesc =
    Desc.documentWithId User <|
        Desc.field "name" .name Desc.string
            >> Desc.field "image" .image Desc.string
            >> Desc.field "email" .email Desc.string


clientDesc : DocumentDesc () Client
clientDesc =
    Desc.document Client <|
        Desc.field "projects"
            .projects
            (Desc.array Desc.reference)


projectDesc : DocumentDesc ProjectSub Project
projectDesc =
    Desc.documentWithIdAndSubs
        Project
        (Desc.field "name" .name Desc.string
            >> Desc.field "members" .members (Desc.list Desc.reference)
            >> Desc.field "admins" .admins (Desc.list Desc.reference)
            >> Desc.field "owner" .owner Desc.reference
            >> Desc.field "proesses" .processes (Desc.idMap processDesc)
            >> Desc.field "parts" .parts (Desc.idMap partDesc)
        )
        ProjectSub
        (Desc.collection "works" .works workDesc)


workDesc : DocumentDesc () Work
workDesc =
    Desc.documentWithId Work <|
        Desc.field "name" .name Desc.string
            >> Desc.field "process" .process Desc.id
            >> Desc.field "belongsTo" .belongsTo (Desc.list Desc.id)
            >> Desc.field "staffs" .staffs (Desc.list Desc.reference)
            >> Desc.field "reviewers" .reviewers (Desc.list Desc.reference)
            >> Desc.optionWithDefault "waitingFor"
                .waitingFor
                []
                (Desc.list <|
                    Desc.map (Desc.iso Ref (\(Ref r) -> r)) Desc.reference
                )


processDesc : Desc Process
processDesc =
    Desc.object Process <|
        Desc.field "name" .name Desc.string
            >> Desc.field "order" .order Desc.float
            >> Desc.field "upstreams" .upstreams (Desc.list Desc.string)


partDesc : Desc Part
partDesc =
    Desc.object Part <|
        Desc.field "name" .name Desc.string
            >> Desc.field "order" .order Desc.float
            >> Desc.option "parent" .parent Desc.string



-- Utilities


myRef : Auth -> Reference () User
myRef auth =
    ref <| Path.fromIds [ "users", unId <| myId auth ]


myId : Auth -> Id x
myId auth =
    Id.fromString auth.uid


gdriveUpdate : GDrive.Update
gdriveUpdate =
    GDrive.update



-- Lenses


clients : Lens Root Data Col (Collection () Client)
clients =
    Lens.collection .clients (\b a -> { a | clients = b })


users : Lens Root Data Col (Collection () User)
users =
    Lens.collection .users (\b a -> { a | users = b })


projects : Lens Root Data Col (Collection ProjectSub Project)
projects =
    Lens.collection .projects (\b a -> { a | projects = b })


myProjects : Auth -> Lens Root Data Col (Collection ProjectSub Project)
myProjects auth =
    o projects <|
        Lens.where_ "members" Lens.CONTAINS Desc.reference (myRef auth)


project : Id Project -> Lens Root Data Doc (Document ProjectSub Project)
project id =
    o projects <| Lens.doc id


myClient : Auth -> Lens Root Data Doc (Document () Client)
myClient auth =
    o clients <| Lens.doc <| myId auth
