module Data.Work exposing (..)

import Data.User as User
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (Desc, DocumentDesc)
import Firestore.Path.Id as Id exposing (Id, SelfId, unId)
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Id.Set as IdSet
import Html exposing (Html, div, input, node, span, text)
import Html.Attributes as Attr exposing (attribute, class, type_)
import List.Extra as List
import Util exposing (boolAttr, flip, icon, onChangeValues)


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
    , waitingFor : List Ref
    }


type alias Reference =
    Firestore.Reference () Work


type Ref
    = Ref Reference


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
    , waitingFor = []
    }


desc : DocumentDesc () Work
desc =
    Desc.documentWithId Work <|
        Desc.field "name" .name Desc.string
            >> Desc.field "process" .process Desc.id
            >> Desc.field "belongsTo" .belongsTo (Desc.list Desc.id)
            >> Desc.field "staffs" .staffs (Desc.list Desc.reference)
            >> Desc.field "reviewers" .reviewers (Desc.list Desc.reference)
            >> Desc.optionWithDefault "waitingFor"
                .waitingFor
                []
                (Desc.list <|
                    Desc.map (Desc.iso Ref (\(Ref r) -> r)) Desc.reference
                )


title : IdMap.Map Process Process -> IdMap.Map Part Part -> Work -> String
title processes parts work =
    let
        process =
            IdMap.get work.process processes
                |> Maybe.withDefault nullProcess

        belongsTo =
            List.filterMap (flip IdMap.get parts) work.belongsTo

        part =
            List.minimumBy .order belongsTo
                |> Maybe.withDefault nullPart

        maxPart =
            List.maximumBy .order belongsTo
                |> Maybe.withDefault nullPart
    in
    if List.length belongsTo == 1 then
        part.name ++ "：" ++ process.name

    else
        part.name
            ++ " 〜 "
            ++ maxPart.name
            ++ "："
            ++ process.name


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


processList :
    IdMap.Map Process Process
    -> List (Id Process)
    -> Html (List (Id Process))
processList processs ids =
    node "ui-dropdown"
        [ class "ui small multiple search selection dropdown select-all"
        , attribute "multiple" ""
        , attribute "value" <| String.join "," <| List.map unId ids
        , onChangeValues |> Attr.map (List.map Id.fromString)
        ]
    <|
        [ input [ type_ "hidden", Attr.name "staffs" ] []
        , Html.i [ class "dropdown icon" ] []
        , div [ class "default text" ] [ text "工程" ]
        , div [ class "menu" ] <|
            List.map
                (\( id, process ) ->
                    div
                        [ class "item"
                        , attribute "data-value" <| unId id
                        ]
                    <|
                        [ span [] [ text process.name ] ]
                )
            <|
                IdMap.toList processs
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
    IdMap.Map Part Part
    -> Work
    -> Html (List (Id Part))
partList parts work =
    let
        values =
            work.belongsTo |> List.map unId
    in
    node "ui-dropdown"
        [ class "ui small multiple search selection dropdown select-all"
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
        , div [ class "default text" ] [ text "カット" ]
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
                IdMap.toList parts
        ]



-- Selection


type alias Selection =
    { works : IdSet.Set Work
    , processes : IdSet.Set Process
    , parts : IdSet.Set Part
    }


isSelected : Selection -> Work -> Bool
isSelected selection work_ =
    IdSet.member (Id.self work_) selection.works
        || IdSet.member work_.process selection.processes
        || List.any (\partId -> IdSet.member partId selection.parts)
            work_.belongsTo


selectionTitle :
    IdMap.Map Process Process
    -> IdMap.Map Part Part
    -> List Work
    -> Selection
    -> String
selectionTitle processes parts works_ selection =
    Maybe.withDefault "" <|
        case IdSet.toList selection.processes of
            [] ->
                case IdSet.toList selection.parts of
                    [] ->
                        case IdSet.toList selection.works of
                            [] ->
                                Nothing

                            [ id ] ->
                                List.find (Id.self >> (==) id) works_
                                    |> Maybe.map (title processes parts)

                            ids ->
                                let
                                    n =
                                        String.fromInt <| List.length ids
                                in
                                Just <| n ++ "件の作業"

                    [ id ] ->
                        IdMap.get id parts |> Maybe.map .name

                    ids ->
                        let
                            selecteds =
                                List.filterMap
                                    (flip IdMap.get parts)
                                    ids
                        in
                        Maybe.map2
                            (\min max -> min.name ++ " 〜 " ++ max.name)
                            (List.minimumBy .order selecteds)
                            (List.maximumBy .order selecteds)

            [ id ] ->
                IdMap.get id processes
                    |> Maybe.map .name

            ids ->
                let
                    selecteds =
                        List.filterMap (flip IdMap.get processes) ids
                in
                Maybe.map2
                    (\min max -> min.name ++ " 〜 " ++ max.name)
                    (List.minimumBy .order selecteds)
                    (List.maximumBy .order selecteds)
