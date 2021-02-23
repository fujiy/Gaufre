module Data.Project exposing (..)

import Data.User as User exposing (User)
import Data.Work as Work exposing (Work)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (DocumentDesc)


type alias Project =
    { name : String
    , members : List (Firestore.Reference () User)
    }


type alias Sub =
    { works : Firestore.Collection () Work
    }


type alias Collection =
    Firestore.Collection Sub Project


type alias Reference =
    Firestore.Reference Sub Project


type alias Document =
    Firestore.Document Sub Project


desc : DocumentDesc Sub Project
desc =
    Desc.documentWithSubs
        Project
        (Desc.field "name" .name Desc.string
            >> Desc.field "members"
                .members
                (Desc.list Desc.reference)
        )
        Sub
        (Desc.collection "works" .works Work.desc)
