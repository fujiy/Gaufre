module Data.Work exposing (..)

import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (DocumentDesc)


type alias ProcessId =
    String


type alias PartId =
    String


type alias Part =
    String


type alias WorkId =
    String


type Status
    = NotAssigned
    | Waiting
    | InProgress
    | Reviewing
    | Complete


type alias Work =
    { name : String
    , process : ProcessId
    , belongsTo : List PartId
    , staffs : List User.Reference
    , status : Status

    -- , reviewers : List (Reference User)
    -- , status : Status
    }


type alias Reference =
    Firestore.Reference () Work


type alias Collection =
    Firestore.Collection () Work


type alias Document =
    Firestore.Document () Work


init : String -> ProcessId -> PartId -> Work
init name process part =
    { name = name
    , process = process
    , belongsTo = [ part ]
    , staffs = []
    , status = NotAssigned
    }


desc : DocumentDesc () Work
desc =
    Desc.document Work <|
        Desc.field "name" .name Desc.string
            >> Desc.field "process" .process Desc.string
            >> Desc.field "belongsTo" .belongsTo (Desc.list Desc.string)
            >> Desc.field "staffs" .staffs (Desc.list Desc.reference)
            >> Desc.field "status"
                .status
                (Desc.enum
                    [ ( "NotAssigned", NotAssigned )
                    , ( "Waiting", Waiting )
                    , ( "InProgress", InProgress )
                    , ( "Reviewing", Reviewing )
                    , ( "Complete", Complete )
                    ]
                )


iconClass : Status -> String
iconClass status =
    (case status of
        NotAssigned ->
            "user times"

        Waiting ->
            "hourglass outline"

        InProgress ->
            "paint brush"

        Reviewing ->
            "eye"

        Complete ->
            "check"
    )
        ++ " icon"
