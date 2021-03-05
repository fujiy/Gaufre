module Data.Work exposing (..)

import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (DocumentDesc)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Util exposing (icon)


type alias Id =
    String


type alias ProcessId =
    String


type alias PartId =
    String


type Status
    = NotAssigned
    | Waiting
    | InProgress
    | Reviewing
    | Complete


type alias Work =
    { id : Id
    , name : String
    , process : ProcessId
    , belongsTo : List PartId
    , staffs : List User.Reference
    , reviewers : List User.Reference
    }


type alias Reference =
    Firestore.Reference () Work


type alias Collection =
    Firestore.Collection () Work


type alias Document =
    Firestore.Document () Work


init : Id -> String -> ProcessId -> PartId -> Work
init id name process part =
    { id = id
    , name = name
    , process = process
    , belongsTo = [ part ]
    , staffs = []
    , reviewers = []
    }


desc : DocumentDesc () Work
desc =
    Desc.documentWithId Work <|
        Desc.field "name" .name Desc.string
            >> Desc.field "process" .process Desc.string
            >> Desc.field "belongsTo" .belongsTo (Desc.list Desc.string)
            >> Desc.field "staffs" .staffs (Desc.list Desc.reference)
            >> Desc.field "reviewers" .reviewers (Desc.list Desc.reference)



-- >> Desc.field "status"
--     .status
--     (Desc.enum
--         [ ( "NotAssigned", NotAssigned )
--         , ( "Waiting", Waiting )
--         , ( "InProgress", InProgress )
--         , ( "Reviewing", Reviewing )
--         , ( "Complete", Complete )
--         ]
--     )


getStatus : Work -> Status
getStatus work =
    if List.isEmpty work.staffs then
        NotAssigned

    else
        Waiting


iconClass : Status -> String
iconClass status =
    case status of
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


statusLabel : Status -> Html msg
statusLabel status =
    let
        ( header, cls ) =
            case status of
                NotAssigned ->
                    ( "担当者がいません", "red" )

                Waiting ->
                    ( "前工程待ち", "" )

                InProgress ->
                    ( "作業中", "" )

                Reviewing ->
                    ( "チェック中", "" )

                Complete ->
                    ( "完了", "green" )
    in
    div [ class "ui label", class cls ]
        [ icon <| iconClass status
        , text header
        ]


statusNumber : Status -> Int
statusNumber status =
    case status of
        NotAssigned ->
            0

        Waiting ->
            1

        InProgress ->
            2

        Reviewing ->
            3

        Complete ->
            4



-- div [ class "ui icon mini message", class cls ]
--     [ icon <| iconClass status
--     , div [ class "content" ]
--         [ div [ class "header" ] [ text header ] ]
--     ]
