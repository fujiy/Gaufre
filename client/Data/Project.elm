module Data.Project exposing (..)

import Data.User as User
import Data.Work as Work
import Dict exposing (Dict)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (Desc, DocumentDesc)
import Firestore.Path exposing (Id)
import GDrive


type alias Project =
    { name : String
    , members : List User.Reference
    , processes : Dict ProcessId Process
    , parts : Dict ProcessId Part
    }


type alias Sub =
    { works : Work.Collection
    }


type alias Collection =
    Firestore.Collection Sub Project


type alias Reference =
    Firestore.Reference Sub Project


type alias Document =
    Firestore.Document Sub Project


init : GDrive.FileMeta -> User.Reference -> Project
init file user =
    { name = file.name
    , members = [ user ]
    , processes = Dict.empty
    , parts = Dict.empty
    }



-- addPart : Project -> Project
-- addPart p =
--     {p | parts }


desc : DocumentDesc Sub Project
desc =
    Desc.documentWithSubs
        Project
        (Desc.field "name" .name Desc.string
            >> Desc.field "members"
                .members
                (Desc.list Desc.reference)
            >> Desc.field "proesses" .processes (Desc.dict processDesc)
            >> Desc.field "parts" .parts (Desc.dict partDesc)
        )
        Sub
        (Desc.collection "works" .works Work.desc)


type alias ProcessId =
    Id


type alias Process =
    { name : String
    , order : Float
    , upstreams : List ProcessId
    }


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


type alias PartId =
    Id


type alias Part =
    { name : String
    , order : Float
    , parent : PartId
    }


partDesc : Desc Part
partDesc =
    Desc.object Part <|
        Desc.field "name" .name Desc.string
            >> Desc.field "order" .order Desc.float
            >> Desc.field "parent" .parent Desc.string
