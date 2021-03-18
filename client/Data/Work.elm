module Data.Work exposing (..)

import Data exposing (..)
import Data.Activity as Activity
import Data.User as User
import Firestore exposing (..)
import Firestore.Access as Access exposing (access)
import Firestore.Desc as Desc
import Firestore.Lens as Lens exposing (QueryOp(..), o, whereEmpty, whereNotEmpty, where_)
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
import Url.Builder as Url
import Util exposing (diff, flip, icon, mean, onChangeValues)


type Status
    = NotAssigned
    | Waiting
    | Working
    | Reviewing
    | Complete


type alias Reference =
    Firestore.Reference WorkSub Work


type alias Collection =
    Firestore.Collection WorkSub Work


type alias Document =
    Firestore.Document WorkSub Work


getWorkProcess : Project -> Work -> Process
getWorkProcess project work =
    IdMap.get work.process project.processes
        |> Maybe.withDefault nullProcess


getWorkPart : Project -> Work -> Part
getWorkPart project =
    getBelongsTo project
        >> List.minimumBy .order
        >> Maybe.withDefault nullPart


getPart : Project -> Id Part -> Part
getPart project id =
    IdMap.get id project.parts |> Maybe.withDefault nullPart


getProcess : Project -> Id Process -> Process
getProcess project id =
    IdMap.get id project.processes |> Maybe.withDefault nullProcess


getBelongsTo : Project -> Work -> List Part
getBelongsTo project work =
    List.filterMap (flip IdMap.get project.parts) work.belongsTo


isMember : Id User -> Work -> Bool
isMember id work =
    let
        user =
            User.ref id
    in
    List.member user work.staffs || List.member user work.reviewers


isWorking : Id User -> Work -> Bool
isWorking id work =
    List.member (User.ref id) work.working


isReviewing : Id User -> Work -> Bool
isReviewing id work =
    List.member (User.ref id) work.reviewing


relativeLink : Project -> Work -> String
relativeLink project work =
    let
        process =
            getWorkProcess project work

        part =
            getWorkPart project work
    in
    Url.relative
        [ process.name, part.name ]
        [ Url.string "work" work.id ]


title : Project -> Work -> String
title project work =
    let
        process =
            getWorkProcess project work

        belongsTo =
            getBelongsTo project work

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


worksTitle :
    Project
    -> IdMap.Map Work Work
    -> List (Id Work)
    -> String
worksTitle project works ids =
    Maybe.withDefault "" <|
        case ids of
            [] ->
                Nothing

            [ id ] ->
                IdMap.get id works
                    |> Maybe.map (title project)

            _ ->
                let
                    n =
                        String.fromInt <| List.length ids
                in
                Just <| n ++ "件の作業"


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


statusColor : Status -> String
statusColor status =
    case status of
        NotAssigned ->
            "red"

        Waiting ->
            ""

        Working ->
            "blue"

        Reviewing ->
            "orange"

        Complete ->
            "green"


statusTitle : Status -> String
statusTitle status =
    case status of
        NotAssigned ->
            "担当者がいません"

        Waiting ->
            "前工程待ち"

        Working ->
            "作業中"

        Reviewing ->
            "チェック中"

        Complete ->
            "完了"


statusLabel : Status -> Html msg
statusLabel status =
    div [ class "ui label", class <| statusColor status ]
        [ icon <| iconClass status, text <| statusTitle status ]


workLabel :
    Project
    -> Work
    -> Html Work
workLabel project work =
    let
        status =
            getStatus work
    in
    Html.a
        [ class "ui label", onClick work ]
        [ icon <| iconClass status, text <| title project work ]


waitingStatuses :
    Project
    -> IdMap.Map Work Work
    -> Work
    -> Html Work
waitingStatuses project works work =
    div [ class "ui horizontal list" ] <|
        flip List.filterMap work.waitingFor <|
            \(WorkRef r) ->
                flip Maybe.map (IdMap.get (getId r) works) <|
                    \waiting ->
                        div [ class "item" ]
                            [ workLabel project waiting ]


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


compare : Project -> Work -> Work -> Order
compare project a b =
    let
        processOrder =
            getWorkProcess project >> .order

        partOrder =
            getBelongsTo project >> List.map .order >> mean
    in
    case Basics.compare (processOrder a) (processOrder b) of
        Basics.EQ ->
            Basics.compare (partOrder a) (partOrder b)

        order ->
            order



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
        [ class "ui small multiple search selection dropdown select-text"
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
        [ class "ui small multiple search selection dropdown select-text"
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
    Project
    -> IdMap.Map Work Work
    -> Selection
    -> String
selectionTitle project works selection =
    Maybe.withDefault "" <|
        case IdSet.toList selection.processes of
            [] ->
                case IdSet.toList selection.parts of
                    [] ->
                        Just <|
                            worksTitle project works <|
                                IdSet.toList selection.works

                    [ id ] ->
                        getPart project id |> .name |> Just

                    ids ->
                        let
                            selecteds =
                                List.map
                                    (getPart project)
                                    ids
                        in
                        Maybe.map2
                            (\min max -> min.name ++ " 〜 " ++ max.name)
                            (List.minimumBy .order selecteds)
                            (List.maximumBy .order selecteds)

            [ id ] ->
                getProcess project id |> .name |> Just

            ids ->
                let
                    selecteds =
                        List.map (getProcess project) ids
                in
                Maybe.map2
                    (\min max -> min.name ++ " 〜 " ++ max.name)
                    (List.minimumBy .order selecteds)
                    (List.maximumBy .order selecteds)



-- Lenses


activities : Lens Doc Document Col Activity.Collection
activities =
    Lens.subCollection .activities (\b a -> { a | activities = b })


ref : Id Project -> Id Work -> Reference
ref pid id =
    Firestore.ref <| Path.fromIds [ "projects", unId pid, "works", unId id ]


processIs : Id Process -> Lens Col Collection Col Collection
processIs id =
    where_ "process" Lens.EQ Desc.id id


partIs : Id Part -> Lens Col Collection Col Collection
partIs id =
    where_ "belongsTo" CONTAINS Desc.id id


processIn : List (Id Process) -> Lens Col Collection Col Collection
processIn id =
    where_ "process" IN Desc.ids id


samePartAs : Work -> Lens Col Collection Col Collection
samePartAs work =
    where_ "belongsTo" CONTAINS_ANY Desc.ids work.belongsTo


downstreamOf : Id Work -> Lens Col Collection Col Collection
downstreamOf id =
    where_ "upstreams" CONTAINS Desc.id id


userIsWorking : Id User -> Lens Col Collection Col Collection
userIsWorking id =
    where_ "working" CONTAINS Desc.reference (User.ref id)


userIsReviewing : Id User -> Lens Col Collection Col Collection
userIsReviewing id =
    where_ "reviewing" CONTAINS Desc.reference (User.ref id)


userIsWaitingUpstream : Id User -> Lens Col Collection Col Collection
userIsWaitingUpstream id =
    o
        (where_ "staffs" CONTAINS Desc.reference (User.ref id))
        (where_ "waitingFor" Lens.NE (Desc.list Desc.string) [])


userIsWaitingSubmit : Id User -> Lens Col Collection Col Collection
userIsWaitingSubmit id =
    o (where_ "reviewers" CONTAINS Desc.reference (User.ref id)) <|
        whereNotEmpty "working"


userIsWaitingReview : Id User -> Lens Col Collection Col Collection
userIsWaitingReview id =
    o (where_ "staffs" CONTAINS Desc.reference (User.ref id)) <|
        whereNotEmpty "reviewing"


isCompletedAsStaff : Id User -> Lens Col Collection Col Collection
isCompletedAsStaff id =
    o (where_ "staffs" CONTAINS Desc.reference (User.ref id)) <|
        o (whereEmpty "waitingFor") <|
            o (whereEmpty "working") <|
                whereEmpty "reviewing"


isCompletedAsReviewer : Id User -> Lens Col Collection Col Collection
isCompletedAsReviewer id =
    o (where_ "reviewers" CONTAINS Desc.reference (User.ref id)) <|
        o (whereEmpty "waitingFor") <|
            o (whereEmpty "working") <|
                whereEmpty "reviewing"



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
    | AddComment (Maybe (Id Activity)) String
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
                                [ "projects"
                                , unId projectId
                                , "works"
                                , unId id
                                ]
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
                            List.filter (getStatus >> (/=) Complete)
                                upstreams
                    in
                    Update.map (\_ -> None) <|
                        Update.default
                            (o worksLens <|
                                Lens.doc <|
                                    Id.fromString folder.id
                            )
                            workDesc
                        <|
                            { id = folder.id
                            , name = folder.name
                            , process = processId
                            , belongsTo = [ partId ]
                            , staffs = []
                            , reviewers = []
                            , upstreams =
                                List.map
                                    (Id.self >> ref projectId >> WorkRef)
                                    upstreams
                            , waitingFor =
                                List.map
                                    (Id.self >> ref projectId >> WorkRef)
                                    waitingFor
                            , working = []
                            , reviewing = []
                            }
                )

        SetStaffs users ->
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
                                work.reviewing
                    in
                    { work
                        | reviewers = reviewers
                        , reviewing = reviewing
                    }

        SetBelongsTo parts ->
            Update.modify lens workDesc <|
                \work -> { work | belongsTo = parts }

        SetUpstreams works ->
            Update.modify lens workDesc <|
                \work ->
                    let
                        upstreams =
                            workRefs works

                        addition =
                            diff upstreams work.upstreams

                        deletion =
                            diff work.upstreams upstreams
                    in
                    if
                        List.member (getStatus work)
                            [ NotAssigned, Waiting ]
                    then
                        let
                            waitingFor =
                                diff work.waitingFor deletion ++ addition

                            working =
                                if List.isEmpty waitingFor then
                                    []

                                else
                                    work.staffs
                        in
                        { work
                            | upstreams = upstreams
                            , waitingFor = waitingFor
                            , working = working
                        }

                    else
                        { work | upstreams = upstreams }

        SetUpstreamProcesses processes ->
            Update.andThen
                (\data ->
                    Access.andThen
                        (\work ->
                            access
                                (o worksLens <|
                                    o (samePartAs work)
                                        Lens.getAll
                                )
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

        AddComment replyTo comment ->
            Update.add (o lens activities) activityDesc <|
                \id ->
                    { id = id
                    , type_ = Comment
                    , createdAt = serverTimestamp
                    , text = comment
                    , author = myRef auth
                    , replyTo =
                        Maybe.map (Activity.ref projectId workId >> ActivityRef)
                            replyTo
                    , reject = False
                    , mentionTo = []
                    }

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
