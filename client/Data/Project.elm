module Data.Project exposing (..)

import Data.User as User
import Data.Work as Work exposing (Work)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (Desc, DocumentDesc)
import Firestore.Lens as Lens exposing (lens, o)
import Firestore.Path exposing (Id(..), SomeId)
import GDrive
import Maybe.Extra as Maybe


type alias Project =
    { id : String
    , name : String
    , members : List User.Reference
    , admins : List User.Reference
    , owner : User.Reference
    , processes : Dict SomeId Process
    , parts : Dict SomeId Part
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
            >> Desc.field "proesses" .processes (Desc.dict processDesc)
            >> Desc.field "parts" .parts (Desc.dict partDesc)
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
    , processes = Dict.empty
    , parts = Dict.empty
    }


makePartName : Int -> String
makePartName i =
    "カット" ++ String.fromInt i


newPart : Project -> ( Id Part, Part )
newPart p =
    let
        last =
            Dict.values p.parts
                |> List.sortBy (.order >> negate)
                |> List.head
                |> Maybe.unwrap 0 .order

        try id name =
            if Dict.member (String.fromInt id) p.parts then
                try (id + 1) name

            else if
                Dict.any
                    (\_ part -> part.name == makePartName name)
                    p.parts
            then
                try id (name + 1)

            else
                ( Id <| String.fromInt id
                , { name = makePartName name
                  , order = last + 1
                  , parent = Nothing
                  }
                )
    in
    try (Dict.size p.parts) (Dict.size p.parts)



-- Process


type alias Process =
    { name : String
    , order : Float
    , upstreams : List SomeId
    }


nullProcess : Process
nullProcess =
    { name = "Null Process", order = 0, upstreams = [] }


processDesc : Desc Process
processDesc =
    Desc.object Process <|
        Desc.field "name" .name Desc.string
            >> Desc.field "order" .order Desc.float
            >> Desc.field "upstreams" .upstreams (Desc.list Desc.string)


defaultProcesses : List Process
defaultProcesses =
    [ { name = "脚本", order = 0, upstreams = [] }
    , { name = "設定", order = 1, upstreams = [] }
    , { name = "絵コンテ", order = 2, upstreams = [] }
    , { name = "レイアウト", order = 3, upstreams = [] }
    , { name = "作画", order = 4, upstreams = [] }
    , { name = "彩色", order = 5, upstreams = [] }
    , { name = "背景", order = 6, upstreams = [] }
    , { name = "撮影", order = 7, upstreams = [] }
    ]



-- Part


type alias Part =
    { name : String
    , order : Float
    , parent : Maybe SomeId
    }


nullPart : Part
nullPart =
    { name = "Null Part", order = 0, parent = Nothing }


partDesc : Desc Part
partDesc =
    Desc.object Part <|
        Desc.field "name" .name Desc.string
            >> Desc.field "order" .order Desc.float
            >> Desc.option "parent" .parent Desc.string
