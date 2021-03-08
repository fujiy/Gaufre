module Data.Work exposing (..)

import Data exposing (..)
import Data.User as User
import Firestore exposing (..)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, unId)
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Id.Set as IdSet
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Html, div, input, node, span, text)
import Html.Attributes as Attr exposing (attribute, class, type_)
import List.Extra as List
import Util exposing (flip, icon, onChangeValues)


type Status
    = NotAssigned
    | Waiting
    | InProgress
    | Reviewing
    | Complete


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
    , waitingFor = []
    }


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



-- Updaters


type Update
    = Add (Id Process) (Id Part) String
    | FolderCreated (Id Process) (Id Part) GDrive.FileMeta
    | SetStaffs (List (Id User))
    | SetReviewers (List (Id User))
    | SetBelongsTo (List (Id Part))
    | Delete
    | None


update :
    Auth
    -> Id Work
    -> (Id Work -> Lens Root Data Doc Document)
    -> Update
    -> Updater Data Update
update auth workId lens_ upd =
    let
        lens =
            lens_ workId
    in
    case upd of
        Add processId partId name ->
            Update.command <|
                \_ ->
                    GDrive.createFolder auth.token
                        name
                        [ unId processId ]
                        |> Cmd.map
                            (Result.map
                                (FolderCreated processId partId)
                                >> Result.withDefault None
                            )

        FolderCreated processId partId folder ->
            Update.set (lens_ <| Id.fromString folder.id) workDesc <|
                init (Id.fromString folder.id)
                    folder.name
                    processId
                    partId

        SetStaffs users ->
            Update.modify lens workDesc <|
                \work -> { work | staffs = userRefs users }

        SetReviewers users ->
            Update.modify lens workDesc <|
                \work -> { work | reviewers = userRefs users }

        SetBelongsTo parts ->
            Update.modify lens workDesc <|
                \work -> { work | belongsTo = parts }

        Delete ->
            Update.all
                [ Update.delete lens workDesc
                , Update.command <|
                    \_ ->
                        GDrive.files_update auth.token
                            (unId workId)
                            { gdriveUpdate | trashed = Just True }
                            |> Cmd.map (\_ -> None)
                ]

        None ->
            Update.none


userRefs : List (Id User) -> List User.Reference
userRefs =
    List.map <|
        \id ->
            Firestore.ref <|
                Path.fromIds [ "users", unId id ]
