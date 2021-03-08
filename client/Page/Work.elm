module Page.Work exposing (..)

import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Data exposing (..)
import Data.Client as Client
import Data.Project as Project
import Data.User as User
import Data.Work as Work
import Dict exposing (Dict)
import Dict.Extra as Dict
import File exposing (File)
import Firestore exposing (Id)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, unId)
import Firestore.Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive exposing (FileMeta)
import Html exposing (Html, button, div, img, input, label, node, text)
import Html.Attributes exposing (attribute, class, hidden, href, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Set exposing (Set)
import Url.Builder as Url
import Util exposing (..)


type alias Model =
    { workId : Id Work
    , folderId : GDrive.Id
    , folder : Maybe FileMeta
    , selection : Set GDrive.Id
    , files : List FileMeta
    , folderCache : Dict GDrive.Id FileMeta
    , loading : Bool
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


init : Id Work -> GDrive.Id -> Model
init workId folderId =
    { workId = workId
    , folderId = folderId
    , folder = Nothing
    , selection = Set.empty
    , files = []
    , folderCache = Dict.empty
    , loading = True
    , downloading = False
    , uploading = False
    , modal = Hidden
    }


type Msg
    = None
    | GotFiles Bool (List FileMeta)
    | RemoveFile FileMeta
    | GotFolder FileMeta
    | MoveToFolder GDrive.Id
    | SelectFile FileMeta Bool Bool
    | ClearSelection
    | ModalState Modal
    | WorkUpdate Work.Update
    | Rename String FileMeta
    | Delete (List FileMeta)
    | Upload (List ( String, File ))
    | CreateFolder String
    | Download (List FileMeta)
    | DownloadReady FileMeta Bytes


initialize : Auth -> Model -> Cmd Msg
initialize auth model =
    let
        id =
            unId model.workId
    in
    if id == model.folderId then
        Cmd.batch
            [ GDrive.filesIn auth.token id
                |> Cmd.map (Result.withDefault [] >> GotFiles False)
            , GDrive.files_get auth.token id
                |> Cmd.map
                    (Result.toMaybe >> Maybe.unwrap None GotFolder)
            ]

    else
        Cmd.batch
            [ GDrive.filesIn auth.token model.folderId
                |> Cmd.map (Result.withDefault [] >> GotFiles False)
            , GDrive.files_get auth.token model.folderId
                |> Cmd.map
                    (Result.toMaybe >> Maybe.unwrap None GotFolder)
            ]


update :
    Auth
    -> Msg
    -> { project : Int }
    -> Model
    -> ( Model, Updater Data Msg )
update auth msg m model =
    let
        projectLens =
            Client.currentProject auth m.project
    in
    case msg of
        GotFiles append files__ ->
            let
                files_ =
                    List.filter (not << .trashed) files__

                files =
                    if append then
                        List.filter
                            (\file ->
                                List.all (\f -> f.id /= file.id) files_
                            )
                            model.files
                            |> List.append files_

                    else
                        files_
            in
            ( { model
                | files = files
                , folderCache =
                    List.filter GDrive.isFolder files
                        |> List.foldr
                            (\folder -> Dict.insert folder.id folder)
                            model.folderCache
                , uploading = False
                , loading = False
              }
            , Update.none
            )

        RemoveFile file ->
            ( { model
                | files = List.filter (.id >> (/=) file.id) model.files
              }
            , Update.none
            )

        GotFolder folder ->
            ( { model
                | folder = Just folder
                , folderCache = Dict.insert folder.id folder model.folderCache
              }
            , Update.none
            )

        MoveToFolder fileId ->
            ( model
            , Update.command <|
                \_ ->
                    Nav.pushUrl auth.navKey <|
                        Url.relative
                            []
                            [ Url.string "work" <| unId <| model.workId
                            , Url.string "folder" fileId
                            ]
            )

        SelectFile file select clear ->
            ( { model
                | selection =
                    (if select then
                        Set.insert

                     else
                        Set.remove
                    )
                        file.id
                        (if clear then
                            Set.empty

                         else
                            model.selection
                        )
              }
            , Update.none
            )

        ModalState modal ->
            ( { model | modal = modal }
            , Update.none
            )

        ClearSelection ->
            ( { model | selection = Set.empty }
            , Update.none
            )

        WorkUpdate upd ->
            ( model
            , let
                lens id =
                    o projectLens <| Project.work id
              in
              Work.update auth model.workId lens upd
                |> Update.map WorkUpdate
            )

        Rename name file ->
            ( model
            , Update.command <|
                \_ ->
                    GDrive.files_update auth.token
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
            , Update.batch <|
                flip List.map files <|
                    \file _ ->
                        GDrive.files_update auth.token
                            file.id
                            { gdriveUpdate | trashed = Just True }
                            |> Cmd.map (\_ -> RemoveFile file)
            )

        CreateFolder name ->
            ( { model | modal = Hidden }
            , Update.command <|
                \_ ->
                    case model.folder of
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
            , Update.command <|
                \_ ->
                    case model.folder of
                        Nothing ->
                            Cmd.none

                        Just parent ->
                            GDrive.uploadFiles auth.token parent.id files
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
            , List.map
                (\file _ ->
                    if GDrive.isFolder file then
                        GDrive.getZip auth.token file
                            |> Cmd.map
                                (Result.unwrap None <|
                                    DownloadReady file
                                )

                    else
                        GDrive.getData auth.token file
                            |> Cmd.map
                                (Result.unwrap None <|
                                    DownloadReady file
                                )
                )
                files
                |> Update.batch
            )

        DownloadReady file bytes ->
            ( { model | downloading = False }
            , Update.command <| \_ -> GDrive.download file bytes
            )

        _ ->
            ( model, Update.none )


userRefs : List (Id User) -> List User.Reference
userRefs =
    List.map <|
        \id ->
            Firestore.ref <|
                Path.fromIds [ "users", unId id ]


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    flip2 Access.map2
        (Access.access
            (o (Data.project <| Id.self project) <|
                o (Project.work model.workId) Lens.get
            )
            data
        )
        (Access.access (o (Project.members project) Lens.gets) data)
    <|
        \work members ->
            let
                editable =
                    Project.myRole project auth
                        |> Project.authority
                        |> .manageWorkStaffs

                ( folders, files ) =
                    List.partition GDrive.isFolder model.files

                staffs =
                    List.map Firestore.getId work.staffs

                reviewers =
                    List.map Firestore.getId work.reviewers

                title =
                    Work.title project.processes project.parts work
            in
            div [ class "ui grid" ]
                [ div [ class "row" ] []
                , div [ class "row" ]
                    [ div [ class "two wide column" ] []
                    , div [ class "twelve wide column" ]
                        [ div [ class "ui fluid card" ]
                            [ div [ class "content" ]
                                [ fileActions model
                                , div [ class "header" ] [ text title ]
                                , div [ class "meta" ]
                                    [ breadcrumb model work
                                        |> Html.map MoveToFolder
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
                                        (fileCard model)
                                        folders
                                , div
                                    [ class "ui link three stackable cards" ]
                                  <|
                                    List.map
                                        (fileCard model)
                                        files
                                , if model.loading then
                                    div [ class "ui basic segment" ]
                                        [ div
                                            [ class <|
                                                "ui active centered"
                                                    ++ "iniline loader"
                                            ]
                                            []
                                        ]

                                  else
                                    when (List.isEmpty model.files) <|
                                        div [ class "center aligned" ]
                                            [ text "ファイルを追加する" ]
                                ]
                            , div [ class "content" ]
                                [ Html.p []
                                    [ text "担当："
                                    , User.list editable members staffs []
                                        |> Html.map
                                            (WorkUpdate << Work.SetStaffs)
                                    ]
                                , Html.p []
                                    [ text "チェック："
                                    , User.list editable members reviewers []
                                        |> Html.map
                                            (WorkUpdate << Work.SetReviewers)
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "two wide column" ] []
                    ]
                , actionModal model
                ]


fileCard : Model -> FileMeta -> Html Msg
fileCard model file =
    let
        selectedOnly =
            (Set.size model.selection == 1)
                && Set.member file.id model.selection

        selected =
            Set.member file.id model.selection

        isFolder =
            GDrive.isFolder file
    in
    div
        [ class "ui card"
        , styleIf selected "background-color" "#E0E0E0"
        , style "transition" "background-color 0.2s"
        , onMouseDownStop <| SelectFile file (not selectedOnly) True
        , attrIf isFolder <| onDoubleClick <| MoveToFolder file.id
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
        [ when (Set.size model.selection == 1) <|
            button
                [ class "ui button"
                , attribute "data-tooltip" "名前を変更"
                , attribute "data-position" "bottom center"
                , onClick <|
                    ModalState <|
                        RenameModal <|
                            Maybe.unwrap "" .name <|
                                Maybe.andThen
                                    (\id ->
                                        List.find
                                            (\file -> file.id == id)
                                            model.files
                                    )
                                    (Set.toList model.selection |> List.head)
                ]
                [ icon "edit" ]
        , when (not <| Set.isEmpty model.selection) <|
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
        , Html.a
            [ class "ui button"
            , attribute "data-tooltip" "Google Drive で開く"
            , attribute "data-position" "bottom center"
            , href <| Maybe.unwrap "" .webViewLink model.folder
            ]
            [ icon "google drive" ]
        , button
            [ class "ui button"
            , classIf model.downloading "loading"
            , attributeIf (not model.downloading) "data-tooltip" "ダウンロード"
            , attribute "data-position" "bottom center"
            , onClick <| ModalState DownloadModal
            ]
            [ icon "download" ]
        ]


breadcrumb : Model -> Work -> Html GDrive.Id
breadcrumb model work =
    let
        path =
            Maybe.unwrap [] back model.folder
                |> (::) ( work.name, work.id )

        back folder =
            if folder.id == work.id then
                []

            else
                List.head folder.parents
                    |> Maybe.andThen
                        (\parent -> Dict.get parent model.folderCache)
                    |> Maybe.unwrap [ ( folder.name, folder.id ) ]
                        (\parent -> ( parent.name, parent.id ) :: back parent)
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
            List.filter
                (\file -> Set.member file.id model.selection)
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
        [ class "ui tiny modal"
        , boolAttr "show" <| model.modal /= Hidden
        , onHide <| ModalState Hidden
        , onApprove <|
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


gdriveUpdate : GDrive.Update
gdriveUpdate =
    GDrive.update
