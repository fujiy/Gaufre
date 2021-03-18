module Data exposing (..)

import Array exposing (Array)
import Browser.Navigation as Nav
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (Desc, DocumentDesc, FirestoreDesc, field, maybe, optional)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, SelfId, unId)
import Firestore.Path.Id.Map as IdMap
import GDrive
import Maybe.Extra as Maybe
import Time



-- Data types


type alias Auth =
    { uid : String
    , token : String
    , navKey : Nav.Key
    , zone : Time.Zone
    , now : Time.Posix
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
    , profile : String
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
    { works : Collection WorkSub Work
    }


type alias Work =
    { id : SelfId
    , name : String
    , process : Id Process
    , belongsTo : List (Id Part)
    , staffs : List (Reference () User)
    , reviewers : List (Reference () User)
    , upstreams : List WorkRef
    , waitingFor : List WorkRef
    , working : List (Reference () User)
    , reviewing : List (Reference () User)
    }


type alias WorkSub =
    { activities : Collection () Activity }


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
    = WorkRef (Reference WorkSub Work)


type alias Activity =
    { id : SelfId
    , type_ : ActivityType
    , createdAt : Timestamp
    , text : String
    , author : Reference () User
    , replyTo : Maybe ActivityRef
    , reject : Bool
    , mentionTo : List (Reference () User)
    }


type ActivityRef
    = ActivityRef (Reference () Activity)


type ActivityType
    = Comment
    | Submit
    | Review



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
        field "name" .name Desc.string
            >> field "image" .image Desc.string
            >> field "email" .email Desc.string
            >> optional "profile" .profile "" Desc.string


clientDesc : DocumentDesc () Client
clientDesc =
    Desc.document Client <|
        field "projects"
            .projects
            (Desc.array Desc.reference)


projectDesc : DocumentDesc ProjectSub Project
projectDesc =
    Desc.documentWithIdAndSubs
        Project
        (field "name" .name Desc.string
            >> field "members" .members Desc.references
            >> field "admins" .admins Desc.references
            >> field "owner" .owner Desc.reference
            >> field "proesses" .processes (Desc.idMap processDesc)
            >> field "parts" .parts (Desc.idMap partDesc)
        )
        ProjectSub
        (Desc.collection "works" .works workDesc)


workRefDesc : Desc WorkRef
workRefDesc =
    Desc.map (Desc.iso WorkRef (\(WorkRef r) -> r)) Desc.reference


workDesc : DocumentDesc WorkSub Work
workDesc =
    Desc.documentWithIdAndSubs Work
        (field "name" .name Desc.string
            >> field "process" .process Desc.id
            >> field "belongsTo" .belongsTo Desc.ids
            >> optional "staffs" .staffs [] Desc.references
            >> optional "reviewers" .reviewers [] Desc.references
            >> optional "upstreams" .upstreams [] (Desc.list workRefDesc)
            >> optional "waitingFor" .waitingFor [] (Desc.list workRefDesc)
            >> optional "working" .working [] Desc.references
            >> optional "reviewing" .reviewing [] Desc.references
        )
        WorkSub
        (Desc.collection "activities" .activities activityDesc)


processDesc : Desc Process
processDesc =
    Desc.object Process <|
        field "name" .name Desc.string
            >> field "order" .order Desc.float
            >> field "upstreams" .upstreams (Desc.list Desc.string)


partDesc : Desc Part
partDesc =
    Desc.object Part <|
        field "name" .name Desc.string
            >> field "order" .order Desc.float
            >> Desc.maybe "parent" .parent Desc.string


activityDesc : DocumentDesc () Activity
activityDesc =
    Desc.documentWithId Activity <|
        field "type"
            .type_
            (Desc.enum
                [ ( "comment", Comment )
                , ( "submit", Submit )
                , ( "review", Review )
                ]
            )
            >> field "createdAt" .createdAt Desc.timestamp
            >> field "text" .text Desc.string
            >> field "author" .author Desc.reference
            >> maybe "replyTo" .replyTo activityRefDesc
            >> field "reject" .reject Desc.bool
            >> field "mentionTo" .mentionTo Desc.references


activityRefDesc : Desc ActivityRef
activityRefDesc =
    Desc.map (Desc.iso ActivityRef (\(ActivityRef r) -> r)) Desc.reference



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
