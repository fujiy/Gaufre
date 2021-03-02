module Page.Browse exposing (..)

import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Data exposing (Auth, Data)
import Data.Project as Project exposing (Part, PartId, Process, ProcessId, Project, newPart, work)
import Data.User as User exposing (User)
import Data.Work as Work exposing (Work)
import Dict exposing (Dict)
import Dict.Extra as Dict
import File exposing (File)
import Firestore exposing (Id)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path
import Firestore.Remote as Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive exposing (FileMeta)
import Html exposing (Html, button, div, img, input, label, node, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, hidden, placeholder, src, style, type_, value)
import Html.Events as Events exposing (onClick, onDoubleClick, onInput)
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Set exposing (Set)
import Url.Builder as Url
import Util exposing (..)
import View.Button as Button


type alias Model =
    { work : Maybe Work.Id
    , workSelection : Set Id
    , fileSelection : Set Id
    , files : Remote (List FileMeta)
    , folder : Maybe FileMeta
    , folderId : Maybe GDrive.Id
    , folderCache : Dict GDrive.Id FileMeta
    , downloading : Bool
    , uploading : Bool
    , modal : Modal
    }


type Modal
    = Hidden
    | DeleteModal
    | RenameModal String
    | UploadModal (List ( String, File ))
    | CreateFolderModal String
    | DownloadModal


init : Model
init =
    { work = Nothing
    , workSelection = Set.empty
    , fileSelection = Set.empty
    , files = Loading
    , folder = Nothing
    , folderId = Nothing
    , folderCache = Dict.empty
    , downloading = False
    , uploading = False
    , modal = Hidden
    }


initWithWork : Maybe Work.Id -> Maybe Work.Id -> Model
initWithWork mwork mfolder =
    { init | work = mwork, folderId = Maybe.or mfolder mwork }


type Msg
    = None
    | GotFiles Bool (List FileMeta)
    | RemoveFile FileMeta
    | GotFolder FileMeta
    | MoveToWork Process Part Work
    | MoveToFolder Process Part Work FileMeta
    | SelectWork Work.Id Bool Bool
    | SelectFile FileMeta Bool Bool
    | ClearSelection
    | ModalState Modal
    | AddPart (List ProcessId) PartId Part
    | CreatedWorkFolder ProcessId PartId FileMeta
    | SetWorkStaffs (List Work) (List User.Id)
    | SetWorkReviewers (List Work) (List User.Id)
    | Rename String FileMeta
    | Delete (List FileMeta)
    | Upload (List ( String, File ))
    | CreateFolder String
    | Download (List FileMeta)
    | DownloadReady FileMeta Bytes


initialize : Auth -> Model -> Cmd Msg
initialize auth model =
    case model.work of
        Nothing ->
            Cmd.none

        Just workId ->
            if model.work == model.folderId then
                Cmd.batch
                    [ GDrive.filesIn auth.token workId
                        |> Cmd.map (Result.withDefault [] >> GotFiles False)
                    , GDrive.files_get auth.token workId
                        |> Cmd.map
                            (Result.toMaybe >> Maybe.unwrap None GotFolder)
                    ]

            else
                Cmd.batch
                    [ GDrive.filesIn auth.token
                        (Maybe.withDefault workId model.folderId)
                        |> Cmd.map (Result.withDefault [] >> GotFiles False)
                    , GDrive.files_get auth.token
                        (Maybe.withDefault workId model.folderId)
                        |> Cmd.map
                            (Result.toMaybe >> Maybe.unwrap None GotFolder)
                    ]


update :
    Auth
    -> Msg
    -> { project : Int }
    -> Model
    -> ( Model, Updater Data, Cmd Msg )
update auth msg m model =
    let
        projectLens =
            Data.currentProject auth m.project
    in
    case msg of
        GotFiles append files__ ->
            let
                files_ =
                    List.filter (not << .trashed) files__

                files =
                    if append then
                        Remote.withDefault [] model.files
                            |> List.filter
                                (\file ->
                                    List.all (\f -> f.id /= file.id) files_
                                )
                            |> List.append files_

                    else
                        files_
            in
            ( { model
                | files = UpToDate files
                , folderCache =
                    List.filter GDrive.isFolder files
                        |> List.foldr
                            (\folder -> Dict.insert folder.id folder)
                            model.folderCache
                , uploading = False
              }
            , Update.none
            , Cmd.none
            )

        RemoveFile file ->
            ( { model
                | files =
                    Remote.map
                        (List.filter (.id >> (/=) file.id))
                        model.files
              }
            , Update.none
            , Cmd.none
            )

        GotFolder folder ->
            ( { model
                | folder = Just folder
                , folderCache = Dict.insert folder.id folder model.folderCache
              }
            , Update.none
            , Cmd.none
            )

        MoveToWork process part work ->
            ( model
            , Update.none
            , Nav.pushUrl auth.navKey <|
                Url.absolute
                    [ String.fromInt m.project, process.name, part.name ]
                    [ Url.string "work" work.id ]
            )

        MoveToFolder process part work file ->
            ( model
            , Update.none
            , Nav.pushUrl auth.navKey <|
                Url.absolute
                    [ String.fromInt m.project, process.name, part.name ]
                    [ Url.string "work" work.id
                    , Url.string "folder" file.id
                    ]
            )

        SelectWork workId select clear ->
            ( { model
                | workSelection =
                    (if select then
                        Set.insert

                     else
                        Set.remove
                    )
                        workId
                        (if clear then
                            Set.empty

                         else
                            model.workSelection
                        )
              }
            , Update.none
            , Cmd.none
            )

        SelectFile file select clear ->
            ( { model
                | fileSelection =
                    (if select then
                        Set.insert

                     else
                        Set.remove
                    )
                        file.id
                        (if clear then
                            Set.empty

                         else
                            model.fileSelection
                        )
              }
            , Update.none
            , Cmd.none
            )

        ModalState modal ->
            ( { model | modal = modal }
            , Update.none
            , Cmd.none
            )

        ClearSelection ->
            ( { model
                | workSelection = Set.empty
                , fileSelection = Set.empty
              }
            , Update.none
            , Cmd.none
            )

        AddPart processes newId newPart ->
            ( model
            , Update.modify projectLens Project.desc <|
                \p ->
                    { p | parts = Dict.insert newId newPart p.parts }
            , List.map
                (\processId ->
                    GDrive.createFolder auth.token
                        newPart.name
                        [ processId ]
                        |> Cmd.map
                            (Result.map (CreatedWorkFolder processId newId)
                                >> Result.withDefault None
                            )
                )
                processes
                |> Cmd.batch
            )

        CreatedWorkFolder processId partId folder ->
            ( model
            , Update.set
                (o projectLens <| Project.work folder.id)
                Work.desc
                (Work.init folder.id folder.name processId partId)
            , Cmd.none
            )

        SetWorkStaffs works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify (o projectLens <| Project.work w.id)
                            Work.desc
                        <|
                            \work -> { work | staffs = userRefs users }
            , Cmd.none
            )

        SetWorkReviewers works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify (o projectLens <| Project.work w.id)
                            Work.desc
                        <|
                            \work -> { work | reviewers = userRefs users }
            , Cmd.none
            )

        Rename name file ->
            ( model
            , Update.none
            , GDrive.files_update auth.token
                file.id
                { gdriveUpdate | name = Just name }
                |> Cmd.map
                    (Result.unwrap None <|
                        List.singleton
                            >> GotFiles True
                    )
            )

        Delete files ->
            ( model
            , Update.none
            , Cmd.batch <|
                flip List.map files <|
                    \file ->
                        GDrive.files_update auth.token
                            file.id
                            { gdriveUpdate | trashed = Just True }
                            |> Cmd.map (\_ -> RemoveFile file)
            )

        CreateFolder name ->
            ( { model | modal = Hidden }
            , Update.none
            , case model.folder of
                Nothing ->
                    Cmd.none

                Just parent ->
                    GDrive.createFolder auth.token name [ parent.id ]
                        |> Cmd.map
                            (Result.unwrap None <|
                                List.singleton
                                    >> GotFiles True
                            )
            )

        Upload files ->
            ( { model
                | uploading =
                    not (List.isEmpty files) || model.uploading
              }
            , Update.none
            , case model.folder of
                Nothing ->
                    Cmd.none

                Just parent ->
                    GDrive.uploadFiles auth.token parent.id files
                        |> Cmd.map (Result.mapError <| Debug.log "ERROR")
                        |> Cmd.map
                            (Result.unwrap None <|
                                List.filterMap
                                    (\( path, file ) ->
                                        if path == "" then
                                            Just file

                                        else
                                            Nothing
                                    )
                                    >> GotFiles True
                            )
            )

        Download files ->
            ( { model
                | downloading =
                    not (List.isEmpty files) || model.downloading
              }
            , Update.none
            , List.map
                (\file ->
                    if GDrive.isFolder file then
                        GDrive.getZip auth.token file
                            |> Cmd.map
                                (Result.unwrap None <| DownloadReady file)

                    else
                        GDrive.getData auth.token file
                            |> Cmd.map
                                (Result.unwrap None <| DownloadReady file)
                )
                files
                |> Cmd.batch
            )

        DownloadReady file bytes ->
            ( { model | downloading = False }
            , Update.none
            , GDrive.download file bytes
            )

        _ ->
            ( model, Update.none, Cmd.none )


userRefs : List User.Id -> List User.Reference
userRefs =
    List.map <|
        \id ->
            Firestore.ref <|
                Path.fromIds [ "users", id ]


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    case model.work of
        Nothing ->
            overview auth model data project

        Just workId ->
            workView auth model data project workId


workView :
    Auth
    -> Model
    -> Data
    -> Project
    -> Work.Id
    -> Accessor Data (Html Msg)
workView auth model data project workId =
    flip2 Access.map2
        (Access.access
            (o (Data.project project.id) <| o (Project.work workId) Lens.get)
            data
        )
        (Access.access (o (Data.projectMembers project) Lens.gets) data)
    <|
        \work members ->
            let
                process =
                    Dict.get work.process project.processes
                        |> Maybe.withDefault Project.nullProcess

                parts =
                    List.filterMap (flip Dict.get project.parts) work.belongsTo

                part =
                    List.minimumBy .order parts
                        |> Maybe.withDefault Project.nullPart

                maxPart =
                    List.maximumBy .order parts
                        |> Maybe.withDefault Project.nullPart

                workName =
                    if List.length parts == 1 then
                        part.name ++ "：" ++ process.name

                    else
                        part.name
                            ++ " 〜 "
                            ++ maxPart.name
                            ++ "："
                            ++ process.name

                ( folders, files ) =
                    Remote.withDefault [] model.files
                        |> List.partition GDrive.isFolder
            in
            div [ class "ui grid" ]
                [ div [ class "row" ] []
                , div [ class "row" ]
                    [ div [ class "two wide column" ] []
                    , div [ class "twelve wide column" ]
                        [ div [ class "ui fluid card" ]
                            [ div [ class "content" ]
                                [ fileActions model
                                , div [ class "header" ] [ text workName ]
                                , div [ class "meta" ]
                                    [ breadcrumb model work
                                        |> Html.map
                                            (Maybe.unwrap
                                                (MoveToWork process part work)
                                                (MoveToFolder process part work)
                                            )
                                    ]
                                ]
                            , div
                                [ class "content"
                                , onMouseDownStop ClearSelection
                                ]
                                [ div
                                    [ class "ui link three stackable cards" ]
                                  <|
                                    List.map
                                        (fileCard model process part work)
                                        folders
                                , div
                                    [ class "ui link three stackable cards" ]
                                  <|
                                    List.map
                                        (fileCard model process part work)
                                        files
                                , case model.files of
                                    Loading ->
                                        div [ class "ui text loader" ]
                                            [ text "Loading" ]

                                    UpToDate [] ->
                                        div [ class "center aligned" ]
                                            [ text "ファイルを追加する" ]

                                    _ ->
                                        text ""
                                ]
                            ]
                        ]
                    , div [ class "two wide column" ] []
                    ]
                , actions members [ work ]
                , actionModal model
                ]


fileCard : Model -> Process -> Part -> Work -> FileMeta -> Html Msg
fileCard model process part work file =
    let
        selectedOnly =
            (Set.size model.fileSelection == 1)
                && Set.member file.id model.fileSelection

        selected =
            Set.member file.id model.fileSelection

        isFolder =
            GDrive.isFolder file
    in
    div
        [ class "ui card"
        , styleIf selected "background-color" "#E0E0E0"
        , style "transition" "background-color 0.2s"
        , onMouseDownStop <| SelectFile file (not selectedOnly) True
        , attrIf isFolder <|
            onDoubleClick <|
                MoveToFolder process part work file
        ]
        [ unless isFolder <|
            div [ class "square image" ]
                [ img [ src file.thumbnailLink ] [] ]
        , div [ class "content" ]
            [ img [ src file.iconLink, class "ui right spaced image" ] []
            , text file.name
            ]
        ]


fileActions : Model -> Html Msg
fileActions model =
    div
        [ class "ui right floated basic icon buttons" ]
        [ when (Set.size model.fileSelection == 1) <|
            button
                [ class "ui button"
                , attribute "data-tooltip" "名前を変更"
                , attribute "data-position" "bottom center"
                , onClick <|
                    ModalState <|
                        RenameModal <|
                            Maybe.unwrap "" .name <|
                                Maybe.andThen2
                                    (\id -> List.find <| \file -> file.id == id)
                                    (Set.toList model.fileSelection
                                        |> List.head
                                    )
                                    (Remote.toMaybe model.files)
                ]
                [ icon "edit" ]
        , when (not <| Set.isEmpty model.fileSelection) <|
            button
                [ class "ui button"
                , attribute "data-tooltip" "削除"
                , attribute "data-position" "bottom center"
                , onClick <| ModalState <| DeleteModal
                ]
                [ icon "trash alternate" ]
        , label
            [ class "ui button"
            , classIf model.uploading "loading"
            , attributeIf (not model.uploading)
                "data-tooltip"
                "ファイルまたはフォルダをアップロード"
            , attribute "data-position" "bottom center"
            ]
            [ icon "cloud upload"
            , input
                [ type_ "file"
                , hidden True
                , attribute "webkitdirectory" ""
                , attribute "multiple" ""
                , onChangeFiles <| ModalState << UploadModal
                ]
                []
            ]
        , button
            [ class "ui button"
            , attribute "data-tooltip" "空のフォルダを作成"
            , attribute "data-position" "bottom center"
            , onClick <| ModalState <| CreateFolderModal ""
            ]
            [ icon "folder open" ]
        , button
            [ class "ui button"
            , classIf model.downloading "loading"
            , attributeIf (not model.downloading) "data-tooltip" "ダウンロード"
            , attribute "data-position" "bottom center"
            , onClick <| ModalState DownloadModal
            ]
            [ icon "download" ]
        ]


breadcrumb : Model -> Work -> Html (Maybe FileMeta)
breadcrumb model work =
    let
        path =
            Maybe.unwrap [] back model.folder
                |> (::) ( work.name, Nothing )

        back folder =
            if folder.id == work.id then
                []

            else
                List.head folder.parents
                    |> Maybe.andThen
                        (\parent -> Dict.get parent model.folderCache)
                    |> Maybe.unwrap [ ( folder.name, Just folder ) ]
                        (\parent -> ( parent.name, Just parent ) :: back parent)
    in
    div [ class "ui breadcrumb" ] <|
        List.intersperse (icon "right angle divider") <|
            flip List.map path <|
                \( name, mfile ) ->
                    Html.a [ class "sction", onClick mfile ]
                        [ text name ]


actionModal : Model -> Html Msg
actionModal model =
    let
        selection =
            Remote.unwrap []
                (List.filter <|
                    \file -> Set.member file.id model.fileSelection
                )
                model.files

        single =
            case selection of
                [ file ] ->
                    Just file

                _ ->
                    Nothing

        includesFile =
            List.any (not << GDrive.isFolder) selection

        includesFolder =
            List.any GDrive.isFolder selection

        title =
            if includesFile && includesFolder then
                "ファイルとフォルダ"

            else if includesFile then
                "ファイル"

            else
                "フォルダ"
    in
    node "ui-modal"
        [ boolAttr "show" <| model.modal /= Hidden
        , class "ui tiny modal"
        , Events.on "hide" <| Decode.succeed <| ModalState Hidden
        , Events.on "approve" <|
            Decode.succeed <|
                case model.modal of
                    RenameModal name ->
                        single |> Maybe.unwrap None (Rename name)

                    UploadModal files ->
                        Upload files

                    CreateFolderModal name ->
                        CreateFolder name

                    DownloadModal ->
                        Download <|
                            if List.isEmpty selection then
                                Maybe.toList model.folder

                            else
                                selection

                    _ ->
                        None
        ]
    <|
        case model.modal of
            RenameModal name ->
                [ div [ class "header" ] [ text <| title ++ "の名前を変更" ]
                , div
                    [ class "content" ]
                    [ div [ class "ui fluid input select-all" ]
                        [ input
                            [ type_ "text"
                            , placeholder <| title ++ "名"
                            , onInput <| RenameModal >> ModalState
                            , value name
                            ]
                            []
                        ]
                    ]
                , div [ class "actions" ]
                    [ div [ class "ui deny button" ] [ text "キャンセル" ]
                    , div
                        [ class "ui positive button"
                        , classIf (name == "") "disabled"
                        ]
                        [ text "変更" ]
                    ]
                ]

            DeleteModal ->
                [ div [ class "header" ]
                    [ text <|
                        (String.fromInt <| List.length selection)
                            ++ ("個の" ++ title ++ "を削除")
                    ]
                , div [ class "actions" ]
                    [ div
                        [ class "ui negative button"
                        , onClick <| Delete selection
                        ]
                        [ text "削除" ]
                    , div
                        [ class "ui deny button"
                        , onClick <| ModalState Hidden
                        ]
                        [ text "キャンセル" ]
                    ]
                ]

            UploadModal files ->
                [ div [ class "header" ]
                    [ text <|
                        String.fromInt (List.length files)
                            ++ "個のファイルをアップロード"
                    ]
                , div [ class "actions" ]
                    [ div [ class "ui deny button" ] [ text "キャンセル" ]
                    , div [ class "ui positive button" ] [ text "アップロード" ]
                    ]
                ]

            CreateFolderModal name ->
                [ div [ class "header" ] [ text "空のフォルダを作成" ]
                , div
                    [ class "content" ]
                    [ div [ class "ui fluid input select-all" ]
                        [ input
                            [ type_ "text"
                            , placeholder "フォルダ名"
                            , onInput <| CreateFolderModal >> ModalState
                            , value name
                            ]
                            []
                        ]
                    ]
                , div [ class "actions" ]
                    [ div [ class "ui deny button" ] [ text "キャンセル" ]
                    , div
                        [ class "ui positive button"
                        , classIf (name == "") "disabled"
                        ]
                        [ text "作成" ]
                    ]
                ]

            DownloadModal ->
                [ div [ class "header" ]
                    [ case List.length selection of
                        0 ->
                            text "フォルダをダウンロード"

                        n ->
                            text <|
                                (String.fromInt n ++ "個の")
                                    ++ (title ++ "をダウンロード")
                    ]
                , div [ class "actions" ]
                    [ div [ class "ui deny button" ] [ text "キャンセル" ]
                    , div [ class "ui positive button" ] [ text "ダウンロード" ]
                    ]
                ]

            _ ->
                []


overview : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
overview auth model data project =
    let
        processes =
            Dict.toList project.processes
                |> List.sortBy (\( _, p ) -> p.order)

        parts =
            Dict.toList project.parts
                |> List.sortBy (\( _, p ) -> p.order)
    in
    flip2 Access.map2
        (Access.access
            (o (Data.project project.id) <| o Project.works Lens.getAll)
            data
        )
        (Access.access (o (Data.projectMembers project) Lens.gets) data)
    <|
        \works_ members ->
            let
                works =
                    List.concatMap
                        (\work ->
                            List.map
                                (\partId ->
                                    ( work.process
                                    , ( partId, work )
                                    )
                                )
                                work.belongsTo
                        )
                        works_
                        |> Dict.groupBy Tuple.first
                        |> Dict.map
                            (\_ ws ->
                                List.map Tuple.second ws
                                    |> Dict.fromList
                            )

                workSelection =
                    List.filter (isSelected model.workSelection) works_
            in
            div
                [ style "min-height" "100vh"
                , style "width" "100%"
                , onMouseDownStop ClearSelection
                ]
                [ table
                    [ class "ui definition celled table select-none"
                    ]
                    [ thead [] [ tableHeader model processes ]
                    , tbody [] <|
                        List.map (tableRow model processes works) parts
                            ++ [ newPartButton project ]
                    ]
                , actions members workSelection
                ]


tableHeader : Model -> List ( ProcessId, Process ) -> Html Msg
tableHeader model processes =
    tr [] <|
        th [] []
            :: List.map
                (\( id, process ) ->
                    let
                        selected =
                            Set.member id model.workSelection
                    in
                    th
                        [ class "selectable"
                        , classIf selected "active"
                        , onMouseDownStop <| SelectWork id (not selected) True
                        , onDragEnter <| SelectWork id (not selected) False
                        ]
                        [ text process.name ]
                )
                processes


tableRow :
    Model
    -> List ( ProcessId, Process )
    -> Dict ProcessId (Dict PartId Work)
    -> ( PartId, Part )
    -> Html Msg
tableRow model processes works ( partId, part ) =
    let
        selected =
            Set.member partId model.workSelection
    in
    tr [] <|
        td
            [ class "selectable right aligned"
            , classIf selected "active"
            , style "padding" "5px"
            , onMouseDownStop <| SelectWork partId (not selected) True
            , onDragEnter <| SelectWork partId (not selected) False
            ]
            [ text part.name ]
            :: List.map
                (\( processId, process ) ->
                    Dict.get processId works
                        |> Maybe.andThen (Dict.get partId)
                        |> Maybe.unwrap emptyCell (workCell model process part)
                )
                processes


emptyCell : Html msg
emptyCell =
    td
        [ class "disabled" ]
        [ icon "plus" ]


workCell : Model -> Process -> Part -> Work -> Html Msg
workCell model process part work =
    let
        selected =
            isSelected model.workSelection work

        selectedOnly =
            model.workSelection == Set.singleton work.id
    in
    td
        [ class "selectable center aligned"
        , classIf selected "active"
        , onMouseDownStop <| SelectWork work.id (not selectedOnly) True
        , onDragEnter <| SelectWork work.id (not selected) False
        , onDoubleClick <| MoveToWork process part work
        ]
        [ icon <| Work.iconClass <| Work.getStatus work ]


actions : List User -> List Work -> Html Msg
actions members workSelection =
    let
        staffs =
            gatherList (.staffs >> List.map Firestore.getId) workSelection

        reviewers =
            gatherList (.reviewers >> List.map Firestore.getId) workSelection

        ( bottom, transition ) =
            if List.isEmpty workSelection then
                ( "-50vh", "" )

            else
                ( "0", "bottom 0.2s ease-out" )
    in
    div
        [ class "ui two column grid"
        , style "position" "fixed"
        , style "width" "calc(100% - 210px)"
        , style "margin" "0"
        , style "bottom" bottom
        , style "transition" transition
        , onMouseDownStop None
        ]
        [ div [ class "column" ]
            [ div [ class "ui fluid card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ text "メンバー" ] ]
                , div [ class "content" ]
                    [ Html.p []
                        [ text "担当："
                        , User.selectionList
                            members
                            (Dict.keys staffs.all)
                            (Dict.keys staffs.partially)
                            |> Html.map (SetWorkStaffs workSelection)
                        ]
                    , Html.p []
                        [ text "チェック："
                        , User.selectionList
                            members
                            (Dict.keys reviewers.all)
                            (Dict.keys reviewers.partially)
                            |> Html.map (SetWorkReviewers workSelection)
                        ]
                    ]
                ]
            ]
        , div [ class "column" ]
            [ div [ class "ui fluid card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ text "スケジュール" ] ]
                ]
            ]
        ]


newPartButton project =
    let
        ( newId, newPart ) =
            Project.newPart project
    in
    tr []
        [ td []
            [ Button.add <|
                AddPart
                    (Dict.keys project.processes)
                    newId
                    newPart
            ]
        ]


isSelected : Set Id -> Work -> Bool
isSelected workSelection work =
    Set.member work.id workSelection
        || Set.member work.process workSelection
        || List.any (flip Set.member workSelection) work.belongsTo


gatherList :
    (Work -> List comparable)
    -> List Work
    ->
        { all : Dict comparable (List Work)
        , partially : Dict comparable (List Work)
        }
gatherList getter =
    List.foldr
        (\work result ->
            let
                items =
                    getter work
                        |> List.map (\item -> ( item, [ work ] ))
                        |> Dict.fromList
            in
            if result.notAll then
                { result
                    | partially =
                        Dict.merge Dict.insert
                            (\item xs ys -> Dict.insert item <| xs ++ ys)
                            Dict.insert
                            items
                            result.partially
                            Dict.empty
                }

            else if Dict.isEmpty items then
                { result
                    | notAll = True
                    , all = Dict.empty
                    , partially =
                        Dict.merge Dict.insert
                            (\item xs ys -> Dict.insert item <| xs ++ ys)
                            Dict.insert
                            result.all
                            result.partially
                            Dict.empty
                }

            else if
                Dict.isEmpty result.all
                    && Dict.isEmpty result.partially
                    && not result.notAll
            then
                { result | all = items }

            else
                Dict.merge
                    (\item _ rs ->
                        { rs
                            | all = Dict.remove item rs.all
                            , partially =
                                Dict.update item
                                    (Maybe.unwrap [ work ] ((::) work) >> Just)
                                    rs.partially
                        }
                    )
                    (\item _ _ rs ->
                        { rs
                            | all =
                                Dict.update item (Maybe.map <| (::) work) rs.all
                        }
                    )
                    (\item _ rs ->
                        { rs
                            | partially =
                                Dict.update item
                                    (Maybe.unwrap [ work ] ((::) work) >> Just)
                                    rs.partially
                        }
                    )
                    result.all
                    items
                    result
        )
        { all = Dict.empty, partially = Dict.empty, notAll = False }
        >> (\r -> { all = r.all, partially = r.partially })


gdriveUpdate : GDrive.Update
gdriveUpdate =
    GDrive.update
