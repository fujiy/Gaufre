module Data.Work exposing (..)

import Data.User as User exposing (User)
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (Desc, DocumentDesc)
import Firestore.Path.Id as Id exposing (Id, IdMap, SelfId, SomeId, unId)
import Html exposing (Html, a, div, img, input, node, span, text)
import Html.Attributes as Attr exposing (attribute, class, src, type_)
import Util exposing (icon, onChangeValues)


type Status
    = NotAssigned
    | Waiting
    | InProgress
    | Reviewing
    | Complete


type alias Work =
    { id : SelfId
    , name : String
    , process : Id Process
    , belongsTo : List (Id Part)
    , staffs : List User.Reference
    , reviewers : List User.Reference
    }


type alias Reference =
    Firestore.Reference () Work


type alias Collection =
    Firestore.Collection () Work


type alias Document =
    Firestore.Document () Work


init : Id Work -> String -> Id Process -> Id Part -> Work
init id name processId partId =
    { id = unId id
    , name = name
    , process = processId
    , belongsTo = [ partId ]
    , staffs = []
    , reviewers = []
    }


desc : DocumentDesc () Work
desc =
    Desc.documentWithId Work <|
        Desc.field "name" .name Desc.string
            >> Desc.field "process" .process Desc.id
            >> Desc.field "belongsTo" .belongsTo (Desc.list Desc.id)
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
-- Process


type alias Process =
    { name : String
    , order : Float
    , upstreams : List SelfId
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
    , parent : Maybe SelfId
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


partList :
    IdMap Part Part
    -> Work
    -> Html (List (Id Part))
partList parts work =
    let
        values =
            work.belongsTo |> List.map unId
    in
    node "ui-dropdown"
        [ class "ui mini multiple search selection dropdown select-all"
        , attribute "multiple" ""
        , attribute "value" <| String.join "," values
        , onChangeValues
            |> Attr.map
                (\ids_ ->
                    let
                        ids =
                            List.map Id.fromString ids_
                    in
                    if List.isEmpty ids then
                        work.belongsTo

                    else
                        ids
                )
        ]
    <|
        [ input [ type_ "hidden", Attr.name "staffs" ] []
        , Html.i [ class "dropdown icon" ] []
        , div [ class "default text" ] [ text "属するカット" ]
        , div [ class "menu" ] <|
            List.map
                (\( id, part ) ->
                    div
                        [ class "item"
                        , attribute "data-value" <| unId id
                        ]
                    <|
                        [ span [] [ text part.name ] ]
                )
            <|
                Id.toList parts
        ]
