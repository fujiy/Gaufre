module Data.Work exposing (..)

import Data exposing (..)
import Dict
import Firestore exposing (..)
import Firestore.Access as Access exposing (access)
import Firestore.Desc as Desc
import Firestore.Lens as Lens exposing (o, where_)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, unId)
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Id.Set as IdSet
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Html, div, input, node, span, text)
import Html.Attributes as Attr exposing (attribute, class, type_)
import Html.Events exposing (onClick)
import List.Extra as List
import Util exposing (diff, flip, icon, onChangeValues)


type Status
    = NotAssigned
    | Waiting
    | Working
    | Reviewing
    | Complete


type alias Reference =
    Firestore.Reference () Work


type alias Collection =
    Firestore.Collection () Work


type alias Document =
    Firestore.Document () Work


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

    else if not <| List.isEmpty work.waitingFor then
        Waiting

    else if not <| List.isEmpty work.working then
        Working

    else if not <| List.isEmpty work.reviewing then
        Reviewing

    else
        Complete


iconClass : Status -> String
iconClass status =
    case status of
        NotAssigned ->
            "user times"

        Waiting ->
            "hourglass outline"

        Working ->
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

                Working ->
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


workLabel :
    IdMap.Map Process Process
    -> IdMap.Map Part Part
    -> Work
    -> Html (Id Work)
workLabel processes parts work =
    let
        status =
            getStatus work
    in
    Html.a
        [ class "ui label"
        , onClick <| Id.self work
        ]
        [ icon <| iconClass status
        , text <| title processes parts work
        ]


waitingStatuses :
    IdMap.Map Process Process
    -> IdMap.Map Part Part
    -> IdMap.Map Work Work
    -> Work
    -> Html (Id Work)
waitingStatuses processes parts works work =
    div [ class "ui horizontal list" ] <|
        flip List.filterMap work.waitingFor <|
            \(WorkRef r) ->
                flip Maybe.map (IdMap.get (getId r) works) <|
                    \waiting ->
                        div [ class "item" ]
                            [ workLabel processes parts waiting ]


statusNumber : Status -> Int
statusNumber status =
    case status of
        NotAssigned ->
            0

        Waiting ->
            1

        Working ->
            2

        Reviewing ->
            3

        Complete ->
            4



-- Process


nullProcess : Process
nullProcess =
    { name = "Null Process", order = 0, upstreams = [] }


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


nullPart : Part
nullPart =
    { name = "Null Part", order = 0, parent = Nothing }


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



-- Lenses


ref : Id Project -> Id Work -> Reference
ref pid id =
    Firestore.ref <| Path.fromIds [ "projects", unId pid, "works", unId id ]


processIs : Id Process -> Lens Col Collection Col Collection
processIs id =
    where_ "process" Lens.EQ Desc.id id


partIs : Id Part -> Lens Col Collection Col Collection
partIs id =
    where_ "belongsTo" Lens.CONTAINS Desc.id id


processIn : List (Id Process) -> Lens Col Collection Col Collection
processIn id =
    where_ "process" Lens.IN Desc.ids id


samePartAs : Work -> Lens Col Collection Col Collection
samePartAs work =
    where_ "belongsTo" Lens.CONTAINS_ANY Desc.ids work.belongsTo


downstreamOf : Id Work -> Lens Col Collection Col Collection
downstreamOf id =
    where_ "upstreams" Lens.CONTAINS Desc.id id



-- Updaters


type Update
    = Add (Id Process) Process (Id Part) String
    | FolderCreated (Id Process) Process (Id Part) GDrive.FileMeta
    | SetStaffs (List (Id User))
    | SetReviewers (List (Id User))
    | SetBelongsTo (List (Id Part))
    | SetUpstreams (List (Id Work))
    | SetUpstreamProcesses (List (Id Process))
    | Delete
    | OtherWork (Id Work) Update
    | AndThen Update Update
    | None


update :
    Auth
    -> Id Project
    -> Id Work
    -> Lens Root Data Col Collection
    -> Update
    -> Updater Data Update
update auth projectId workId worksLens upd =
    let
        lens =
            o worksLens <| Lens.doc workId

        userRefs =
            List.map <|
                \id ->
                    Firestore.ref <|
                        Path.fromIds [ "users", unId id ]

        workRefs =
            List.map <|
                \id ->
                    WorkRef <|
                        Firestore.ref <|
                            Path.fromIds
                                [ "projects", unId projectId, "works", unId id ]
    in
    case upd of
        Add processId process partId name ->
            Update.command <|
                \_ ->
                    GDrive.createFolder auth.token
                        name
                        [ unId processId ]
                        |> Cmd.map
                            (Result.map
                                (FolderCreated processId process partId)
                                >> Result.withDefault None
                            )

        FolderCreated processId process partId folder ->
            Update.andThen
                (access <| (o worksLens <| o (partIs partId) Lens.getAll))
                (\ws ->
                    let
                        upstreams =
                            List.filter
                                (\w ->
                                    List.member (unId w.process)
                                        process.upstreams
                                )
                                ws

                        waitingFor =
                            List.filter (getStatus >> (/=) Complete) upstreams
                    in
                    Update.map (\_ -> None) <|
                        Update.default
                            (o worksLens <| Lens.doc <| Id.fromString folder.id)
                            workDesc
                        <|
                            { id = folder.id
                            , name = folder.name
                            , process = processId
                            , belongsTo = [ partId ]
                            , staffs = []
                            , reviewers = []
                            , upstreams =
                                List.map (Id.self >> ref projectId >> WorkRef)
                                    upstreams
                            , waitingFor =
                                List.map (Id.self >> ref projectId >> WorkRef)
                                    waitingFor
                            , working = []
                            , reviewing = []
                            }
                )

        SetStaffs users ->
            Update.map (\_ -> None) <|
                Update.modify lens workDesc <|
                    \work ->
                        let
                            staffs =
                                userRefs users

                            addition =
                                diff staffs work.staffs

                            deletion =
                                diff work.staffs staffs

                            working =
                                if
                                    List.member (getStatus work)
                                        [ NotAssigned, Working ]
                                then
                                    diff work.working deletion ++ addition

                                else
                                    work.working
                        in
                        { work | staffs = staffs, working = working }

        SetReviewers users ->
            Update.map (\_ -> None) <|
                Update.modify lens workDesc <|
                    \work ->
                        let
                            reviewers =
                                userRefs users

                            addition =
                                diff reviewers work.reviewers

                            deletion =
                                diff work.reviewers reviewers

                            reviewing =
                                if getStatus work == Reviewing then
                                    diff work.reviewing deletion ++ addition

                                else
                                    work.working
                        in
                        { work | reviewers = reviewers, reviewing = reviewing }

        SetBelongsTo parts ->
            Update.map (\_ -> None) <|
                Update.modify lens workDesc <|
                    \work -> { work | belongsTo = parts }

        SetUpstreams works ->
            Update.map (\_ -> None) <|
                Update.modify lens workDesc <|
                    \work ->
                        let
                            upstreams =
                                workRefs works

                            addition =
                                diff upstreams work.upstreams

                            deletion =
                                diff work.upstreams upstreams

                            waitingFor =
                                if
                                    List.member (getStatus work)
                                        [ NotAssigned, Waiting ]
                                then
                                    diff work.waitingFor deletion ++ addition

                                else
                                    work.waitingFor
                        in
                        { work
                            | upstreams = upstreams
                            , waitingFor = waitingFor
                        }

        SetUpstreamProcesses processes ->
            Update.andThen
                (\data ->
                    Access.andThen
                        (\work ->
                            access
                                (o worksLens <| o (samePartAs work) Lens.getAll)
                                data
                        )
                        (access
                            (o worksLens <| o (Lens.doc workId) Lens.get)
                            data
                        )
                )
                (\ws ->
                    List.filter (\w -> List.member w.process processes) ws
                        |> List.map Id.self
                        |> SetUpstreams
                        |> Update.succeed
                )

        Delete ->
            Update.all
                [ Update.delete lens workDesc
                , Update.andThen
                    (access
                        (o worksLens <| o (downstreamOf workId) Lens.getAll)
                    )
                    (flip Update.for <|
                        \work ->
                            Update.succeed <|
                                OtherWork (Id.self work) <|
                                    SetUpstreams <|
                                        List.remove workId <|
                                            List.map
                                                (\(WorkRef r) -> getId r)
                                                work.upstreams
                    )
                , Update.command <|
                    \_ ->
                        GDrive.files_update auth.token
                            (unId workId)
                            { gdriveUpdate | trashed = Just True }
                            |> Cmd.map (\_ -> None)
                ]

        OtherWork workId_ upd_ ->
            update auth projectId workId_ worksLens upd_
                |> Update.map (OtherWork workId_)

        AndThen snd None ->
            update auth projectId workId worksLens snd

        AndThen snd fst ->
            update auth projectId workId worksLens fst
                |> Update.map (AndThen snd)

        None ->
            Update.none
