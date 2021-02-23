module Data.Work exposing (..)

import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (DocumentDesc)


type alias Part =
    String


type Status
    = NotAssigned
    | Waiting
    | InProgress
    | Reviewing
    | Complete


type alias Work =
    { name : String

    -- , process : String
    -- , belongsTo : List Part
    -- , staffs : List (Reference User)
    -- , reviewers : List (Reference User)
    -- , status : Status
    }


desc : DocumentDesc () Work
desc =
    Desc.document Work <|
        Desc.field "name" .name Desc.string
