module Page.Projects exposing (..)

import Array
import Array.Extra as Array
import Data exposing (Auth, Data, project)
import Data.Client as Client
import Data.Project as Project exposing (Project)
import Data.Work as Work exposing (Process)
import Dict
import Firestore exposing (..)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id)
import Firestore.Remote as Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Html, a, button, div, input, node, text)
import Html.Attributes exposing (class, href, placeholder, style, type_)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import List
import List.Extra as List
import Maybe.Extra as Maybe
import Util exposing (..)


type Model
    = List
    | SearchFolder
        { loading : Bool
        , wait : Maybe String
        , result : List GDrive.FileMeta
        }


type Msg
    = ShowModal
    | HideModal
    | Search String
    | SearchResult (List GDrive.FileMeta)
    | JoinProject Project
    | AddProject GDrive.FileMeta
    | AddProjectProcess (Id Project) Process GDrive.FileMeta
    | None


init : Model
init =
    List


update : Auth -> Msg -> Model -> ( Model, Updater Data Msg )
update auth msg model =
    case ( msg, model ) of
        ( ShowModal, _ ) ->
            ( SearchFolder { loading = False, wait = Nothing, result = [] }
            , Update.none
            )

        ( HideModal, _ ) ->
            ( List, Update.none )

        ( Search "", _ ) ->
            ( model, Update.none )

        ( Search s, SearchFolder o ) ->
            if o.loading then
                ( SearchFolder { o | wait = Just s }
                , Update.none
                )

            else
                ( SearchFolder { o | loading = True, wait = Nothing }
                , Update.command <|
                    \_ ->
                        GDrive.folders auth.token s
                            |> Cmd.map
                                (\r ->
                                    case r of
                                        Err error ->
                                            always (SearchResult []) <|
                                                Debug.log "error" error

                                        Ok files ->
                                            SearchResult files
                                )
                )

        ( SearchResult files, SearchFolder o ) ->
            case o.wait of
                Nothing ->
                    ( SearchFolder { o | loading = False, result = files }
                    , Update.none
                    )

                Just s ->
                    ( SearchFolder
                        { o | loading = True, wait = Nothing, result = files }
                    , Update.command <|
                        \_ ->
                            GDrive.folders auth.token s
                                |> Cmd.map
                                    (\r ->
                                        case r of
                                            Err error ->
                                                always (SearchResult []) <|
                                                    Debug.log "error" error

                                            Ok files_ ->
                                                SearchResult files_
                                    )
                    )

        ( JoinProject project, _ ) ->
            ( model
            , Update.modify (Data.myClient auth) Client.desc <|
                \client ->
                    { client
                        | projects =
                            Array.push (Data.projectRef <| Id.self project)
                                client.projects
                    }
            )

        ( AddProject file, _ ) ->
            ( model
            , Update.all
                [ Update.modify (Data.myClient auth) Client.desc <|
                    \client ->
                        { client
                            | projects =
                                Array.push
                                    (Data.projectRef <| Id.fromString file.id)
                                    client.projects
                        }
                , Update.set
                    (o Data.projects <| Lens.doc <| Id.fromString file.id)
                    Project.desc
                  <|
                    Project.init file <|
                        Data.myRef auth
                , Update.batch <|
                    flip List.map Work.defaultProcesses <|
                        \process _ ->
                            GDrive.createFolder
                                auth.token
                                process.name
                                [ file.id ]
                                |> Cmd.map
                                    (Result.map
                                        (AddProjectProcess
                                            (Id.fromString file.id)
                                            process
                                        )
                                        >> Result.withDefault None
                                    )
                ]
            )

        ( AddProjectProcess pid process file, _ ) ->
            ( model
            , Update.modify
                (o Data.projects <| Lens.doc pid)
                Project.desc
              <|
                \project ->
                    { project
                        | processes =
                            Id.insert (Id.fromString file.id)
                                process
                                project.processes
                    }
            )

        _ ->
            ( model, Update.none )


remote : Remote a -> (a -> Html msg) -> Html msg
remote r f =
    case r of
        Loading ->
            div [ class "ui active centered inline loader" ] []

        Failure ->
            div [ class "ui negative message" ]
                [ icon "exclamation circle" ]

        Committing a ->
            div [ class "ui active inverted dimmer" ]
                [ div [ class "ui text loader" ] []
                , f a
                ]

        UpToDate a ->
            f a


view : Auth -> Model -> Data -> Accessor Data (Html Msg)
view auth model data =
    flip2 Access.andThen2
        (Access.access
            (o (Data.myProjects auth) Lens.getAll)
            data
        )
        (Access.access (o (Data.myClient auth) Lens.get) data)
    <|
        \projects client ->
            let
                workings =
                    List.filterMap
                        (\p ->
                            Array.toList client.projects
                                |> List.findIndex
                                    (\ref -> Firestore.getId ref == Id.self p)
                                |> Maybe.map
                                    (\i -> ( i, p ))
                        )
                        projects
                        |> List.sortBy Tuple.first

                inviteds =
                    flip List.filter projects <|
                        \p ->
                            Array.toList client.projects
                                |> List.any
                                    (\ref -> Firestore.getId ref == Id.self p)
                                |> not
            in
            Access.map
                (\html ->
                    div []
                        [ div [ class "ui cards", style "margin" "20px" ] <|
                            List.map projectCard workings
                                ++ List.map invitedProjectCard inviteds
                                ++ [ addProjectCard ]
                        , html
                        ]
                )
            <|
                searchModal auth data model


projectCard ( i, project ) =
    a
        [ class "ui card centered"
        , href <| "/" ++ String.fromInt i ++ "/dashboard"
        ]
        [ div [ class "content" ]
            [ div [ class "center aligned header" ] [ text project.name ]
            ]
        ]


invitedProjectCard project =
    div
        [ class "ui card centered" ]
        [ div [ class "content" ]
            [ div [ class "center aligned header" ] [ text project.name ]
            , div [ class "center aligned description" ]
                [ text "プロジェクトに招待されています" ]
            ]
        , div
            [ class "ui bottom attached primary button"
            , onClick <| JoinProject project
            ]
            [ icon "sign-in", text "参加する" ]
        ]


addProjectCard =
    a
        [ class "ui card centered"
        , onClick ShowModal
        ]
        [ div [ class "content" ]
            [ div [ class "center aligned header" ]
                [ Html.i [ class "plus icon", style "margin" "20px" ] [] ]
            , div [ class "center aligned header" ]
                [ text "プロジェクトを追加" ]
            ]
        ]


searchModal : Auth -> Data -> Model -> Accessor Data (Html Msg)
searchModal auth data model =
    let
        showModal =
            case model of
                SearchFolder _ ->
                    True

                _ ->
                    False
    in
    Access.map
        (\html ->
            node "ui-modal"
                [ boolAttr "show" showModal
                , class "ui tiny modal"
                , on "hide" <| Decode.succeed HideModal
                ]
                [ div [ class "header" ]
                    [ text "Google Drive内からプロジェクトフォルダを選択" ]
                , html
                ]
        )
    <|
        case model of
            SearchFolder { result, loading } ->
                Access.map
                    (\pl ->
                        div [ class "content" ]
                            [ div
                                [ class "ui fluid search"
                                , classIf loading "loading"
                                ]
                                [ div [ class "ui icon fluid input select-all" ]
                                    [ input
                                        [ class "propt"
                                        , type_ "text"
                                        , placeholder "Folder names..."
                                        , onInput Search
                                        ]
                                        []
                                    , icon "search"
                                    ]
                                ]
                            , pl
                            ]
                    )
                <|
                    projectList auth data <|
                        List.filter (not << .trashed) result

            _ ->
                Access.success <| text ""


projectList : Auth -> Data -> List GDrive.FileMeta -> Accessor Data (Html Msg)
projectList auth data files =
    Access.maps (div [ class "ui divided items" ]) <|
        flip List.map files <|
            \file ->
                Access.accessMapMaybe
                    (o Data.projects <|
                        o (Lens.doc <| Id.fromString file.id) Lens.get
                    )
                    data
                <|
                    \mproject ->
                        let
                            userRef =
                                Firestore.ref <|
                                    Path.fromIds [ "users", auth.uid ]

                            status =
                                Maybe.map
                                    (\p -> List.member userRef p.members)
                                    mproject
                        in
                        div [ class "item" ]
                            [ div [ class "middle aligned content" ]
                                [ Html.h4 [ class "ui header" ]
                                    [ text file.name ]
                                , button
                                    [ class
                                        "ui right floated basic primary button"
                                    , classIf (Maybe.isJust status) "disabled"
                                    , onClick <| AddProject file
                                    ]
                                  <|
                                    case status of
                                        Nothing ->
                                            [ icon "plus"
                                            , text "新しく始める"
                                            ]

                                        Just False ->
                                            [ icon "users"
                                            , text "招待が必要"
                                            ]

                                        Just True ->
                                            [ icon "check"
                                            , text "参加済み"
                                            ]
                                ]
                            ]
