module Data.Project exposing (..)

import Data.User as User
import Data.Work as Work exposing (Part, Process, Work)
import Dict.Extra as Dict
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (Desc, DocumentDesc)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path.Id as Id exposing (Id, IdMap, SelfId)
import GDrive
import Maybe.Extra as Maybe


type alias Project =
    { id : SelfId
    , name : String
    , members : List User.Reference
    , admins : List User.Reference
    , owner : User.Reference
    , processes : IdMap Process Process
    , parts : IdMap Part Part
    }


type Role
    = Owner
    | Admin
    | Staff


type alias Authority =
    { manageMembers : Bool
    , manageWorkStaffs : Bool
    , editStructure : Bool
    , deleteProject : Bool
    }


staffAuthority : Authority
staffAuthority =
    Authority False False False False


adminAuthority : Authority
adminAuthority =
    { staffAuthority
        | manageMembers = True
        , manageWorkStaffs = True
        , editStructure = True
    }


ownerAuthority : Authority
ownerAuthority =
    { adminAuthority | deleteProject = True }


authority : Role -> Authority
authority role =
    case role of
        Owner ->
            ownerAuthority

        Admin ->
            adminAuthority

        Staff ->
            staffAuthority



-- Firestore


type alias Sub =
    { works : Work.Collection
    }


type alias Collection =
    Firestore.Collection Sub Project


type alias Reference =
    Firestore.Reference Sub Project


type alias Document =
    Firestore.Document Sub Project


desc : DocumentDesc Sub Project
desc =
    Desc.documentWithIdAndSubs
        Project
        (Desc.field "name" .name Desc.string
            >> Desc.field "members" .members (Desc.list Desc.reference)
            >> Desc.field "admins" .admins (Desc.list Desc.reference)
            >> Desc.field "owner" .owner Desc.reference
            >> Desc.field "proesses" .processes (Desc.idMap Work.processDesc)
            >> Desc.field "parts" .parts (Desc.idMap Work.partDesc)
        )
        Sub
        (Desc.collection "works" .works Work.desc)



-- Lenses


works : Lens Doc Document Col Work.Collection
works =
    Lens.subCollection .works (\b a -> { a | works = b })


work : Id Work -> Lens Doc Document Doc Work.Document
work id =
    o works <| Lens.doc id



-- Utilities


init : GDrive.FileMeta -> User.Reference -> Project
init file user =
    { id = file.id
    , name = file.name
    , members = [ user ]
    , admins = [ user ]
    , owner = user
    , processes = Id.empty
    , parts = Id.empty
    }


makePartName : Int -> String
makePartName i =
    "カット" ++ String.fromInt i


newPart : Project -> ( Id Part, Part )
newPart p =
    let
        last =
            Id.items p.parts
                |> List.sortBy (.order >> negate)
                |> List.head
                |> Maybe.unwrap 0 .order

        try id name =
            if Id.member (Id.fromString <| String.fromInt id) p.parts then
                try (id + 1) name

            else if
                Id.any
                    (\part -> part.name == makePartName name)
                    p.parts
            then
                try id (name + 1)

            else
                ( Id.fromString <| String.fromInt id
                , { name = makePartName name
                  , order = last + 1
                  , parent = Nothing
                  }
                )
    in
    try (Id.size p.parts) (Id.size p.parts + 1)
