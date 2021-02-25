module Page.Projects exposing (..)

import Array
import Array.Extra as Array
import Data exposing (Auth, Data, project)
import Data.Client as Client
import Data.Project as Project
import Dict
import Firestore exposing (..)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path exposing (Id)
import Firestore.Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Html, a, button, div, i, input, node, text)
import Html.Attributes exposing (class, href, placeholder, style, type_)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
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
    | AddProject Bool GDrive.FileMeta
    | AddProjectProcess Id Project.Process GDrive.FileMeta
    | None


init : Model
init =
    List


update : Auth -> Msg -> Model -> ( Model, Updater Data, Cmd Msg )
update auth msg model =
    case ( msg, model ) of
        ( ShowModal, _ ) ->
            ( SearchFolder { loading = False, wait = Nothing, result = [] }
            , Update.none
            , Cmd.none
            )

        ( HideModal, _ ) ->
            ( List, Update.none, Cmd.none )

        ( Search "", _ ) ->
            ( model, Update.none, Cmd.none )

        ( Search s, SearchFolder o ) ->
            if o.loading then
                ( SearchFolder { o | wait = Just s }
                , Update.none
                , Cmd.none
                )

            else
                ( SearchFolder { o | loading = True, wait = Nothing }
                , Update.none
                , GDrive.folders auth.token s
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
                    , Cmd.none
                    )

                Just s ->
                    ( SearchFolder
                        { o | loading = True, wait = Nothing, result = files }
                    , Update.none
                    , GDrive.folders auth.token s
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

        ( AddProject exists file, _ ) ->
            let
                projectRef =
                    Firestore.ref <| Path.fromIds [ "projects", file.id ]

                userRef =
                    Firestore.ref <| Path.fromIds [ "users", auth.uid ]
            in
            ( model
            , Update.all
                [ Update.modify (Data.myClient auth) Client.desc <|
                    \client ->
                        { client
                            | projects =
                                Array.push projectRef client.projects
                        }
                , Update.alter
                    (o Data.projects <| Lens.doc file.id)
                    Project.desc
                  <|
                    \mp ->
                        Update.Update <|
                            case mp of
                                Nothing ->
                                    Project.init file userRef

                                Just p ->
                                    { p
                                        | members =
                                            userRef :: p.members
                                    }
                ]
            , if exists then
                Cmd.none

              else
                Cmd.batch <|
                    flip List.map Project.defaultProcesses <|
                        \process ->
                            GDrive.createFolder
                                auth.token
                                process.name
                                [ file.id ]
                                |> Cmd.map
                                    (Result.map
                                        (AddProjectProcess file.id process)
                                        >> Result.mapError (Debug.log "ERR")
                                        >> Result.withDefault None
                                    )
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
                            Dict.insert file.id process project.processes
                    }
            , Cmd.none
            )

        _ ->
            ( model, Update.none, Cmd.none )


remote : Remote a -> (a -> Html msg) -> Html msg
remote r f =
    case r of
        Loading ->
            div [ class "ui active centered inline loader" ] []

        Failure ->
            div [ class "ui negative message" ]
                [ i [ class "exclamation circle icon" ] [] ]

        Committing a ->
            div [ class "ui active inverted dimmer" ]
                [ div [ class "ui text loader" ] []
                , f a
                ]

        UpToDate a ->
            f a


view : Auth -> Model -> Data -> Accessor Data (Html Msg)
view auth model data =
    flip Access.andThen
        (Access.access
            (o (Data.myClient auth) <|
                o Lens.get Client.projects
            )
            data
            |> Access.map Array.toList
            |> Access.for
                (Lens.derefAndAccess Data.project data
                    >> Access.thenAccess Lens.get
                    >> Access.remote
                )
            |> Access.map (List.indexedMap Tuple.pair)
        )
    <|
        \projects ->
            Access.map
                (\html ->
                    div []
                        [ div [ class "ui cards", style "margin" "20px" ] <|
                            List.append
                                (List.map projectCard projects)
                                [ addProjectCard ]
                        , html
                        ]
                )
            <|
                searchModal auth data model


projectCard ( i, rp ) =
    a
        [ class "ui card centered"
        , href <|
            "/"
                ++ String.fromInt i
                ++ "/dashboard"
        ]
        [ div [ class "content" ]
            [ remote rp <|
                \project ->
                    div
                        [ class
                            "center aligned header"
                        ]
                        [ text project.name ]
            ]
        ]


addProjectCard =
    a
        [ class "ui card centered"
        , onClick ShowModal
        ]
        [ div [ class "content" ]
            [ div [ class "center aligned header" ]
                [ i
                    [ class "plus icon"
                    , style "margin" "20px"
                    ]
                    []
                ]
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
                                [ div [ class "ui icon fluid input" ]
                                    [ input
                                        [ class "propt"
                                        , type_ "text"
                                        , placeholder "Folder names..."
                                        , onInput Search
                                        ]
                                        []
                                    , i [ class "search icon" ] []
                                    ]
                                ]
                            , pl
                            ]
                    )
                <|
                    projectList auth data result

            _ ->
                Access.success <| text ""


projectList : Auth -> Data -> List GDrive.FileMeta -> Accessor Data (Html Msg)
projectList auth data files =
    Access.maps (div [ class "ui divided items" ]) <|
        flip List.map files <|
            \file ->
                Access.accessMapMaybe
                    (o Data.projects <|
                        o (Lens.doc file.id) Lens.get
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
                                    , classIf (status == Just True)
                                        "disabled"
                                    , onClick <|
                                        AddProject (Maybe.isJust status) file
                                    ]
                                  <|
                                    case status of
                                        Nothing ->
                                            [ i [ class "plus icon" ] []
                                            , text "新しく始める"
                                            ]

                                        Just False ->
                                            [ i [ class "sign-in icon" ] []
                                            , text "参加する"
                                            ]

                                        Just True ->
                                            [ i [ class "check icon" ] []
                                            , text "参加済み"
                                            ]
                                ]
                            ]
