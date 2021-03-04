module Data.Client exposing (..)

import Array exposing (Array)
import Data.Project as Project exposing (Project)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (DocumentDesc)
import Firestore.Lens as Lens


type alias Client =
    { projects : Array (Reference Project.Sub Project)
    }


type alias Collection =
    Firestore.Collection () Client


type alias Document =
    Firestore.Document () Client


desc : DocumentDesc () Client
desc =
    Desc.document Client <|
        Desc.field "projects"
            .projects
            (Desc.array Desc.reference)


projects : Lens Item Client Item (Array (Reference Project.Sub Project))
projects =
    Lens.lens .projects (\ps c -> { c | projects = ps })
