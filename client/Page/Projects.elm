module Page.Projects exposing (..)

import Array
import Array.Extra as Array
import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Data.Project as Project
import Data.User as User exposing (Project, User)
import Firestore
import Firestore.Access as Access exposing (Accessor)
import Firestore.Types exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Html, a, div, i, input, node, span, text)
import Html.Attributes exposing (attribute, class, href, placeholder, style, type_)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
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
    | AddProject GDrive.FileMeta


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

        ( AddProject file, _ ) ->
            let
                projectRef =
                    Firestore.ref [ "projects", file.id ]

                userRef =
                    Firestore.ref [ "users", auth.uid ]
            in
            ( model
            , Update.updates
                [ Update.collection Data.usersLens <|
                    Update.doc auth.uid <|
                        Update.modify User.encode <|
                            \user ->
                                { user
                                    | projects =
                                        Array.push projectRef user.projects
                                }
                , Update.collection Data.projectsLens <|
                    Update.doc file.id <|
                        Update.alter Project.encode <|
                            \mp ->
                                Just <|
                                    case mp of
                                        Nothing ->
                                            User.Project
                                                { name = file.name
                                                , members = [ userRef ]
                                                }

                                        Just (User.Project p) ->
                                            User.Project
                                                { p
                                                    | members =
                                                        userRef :: p.members
                                                }
                ]
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


view : Auth -> Model -> Data -> User -> Accessor (Html Msg)
view auth model data user =
    flip Access.map
        (Array.mapToList Access.get_ user.projects
            |> Access.list
            |> Access.map (List.indexedMap Tuple.pair)
        )
    <|
        -- Access.just <| flip identity [] <|
        \projects ->
            div []
                [ div [ class "ui cards", style "margin" "20px" ] <|
                    List.append
                        (flip List.map
                            projects
                            (\( i, rp ) ->
                                a
                                    [ class "ui card centered"
                                    , href <| "/" ++ String.fromInt i
                                    ]
                                    [ div [ class "content" ]
                                        [ remote rp <|
                                            \(User.Project project) ->
                                                div
                                                    [ class
                                                        "center aligned header"
                                                    ]
                                                    [ text project.name ]
                                        ]
                                    ]
                            )
                        )
                        [ a
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
                                    [ text "Add Project" ]
                                ]
                            ]
                        ]
                , searchModal auth data model
                ]


searchModal : Auth -> Data -> Model -> Html Msg
searchModal auth data model =
    let
        showModal =
            case model of
                SearchFolder _ ->
                    True

                _ ->
                    False
    in
    node "ui-modal"
        [ boolAttr "show" showModal
        , class "ui tiny modal"
        , on "hide" <| Decode.succeed HideModal
        ]
        [ div [ class "header" ]
            [ text "Google Drive内のフォルダからプロジェクトフォルダを選択" ]
        , case model of
            SearchFolder { result, loading } ->
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
                    , div [ class "ui link items" ] <|
                        flip List.map result <|
                            \file ->
                                div [ class "item" ]
                                    [ a
                                        [ class "header"
                                        , onClick <| AddProject file
                                        ]
                                        [ text file.name ]
                                    ]
                    ]

            _ ->
                text ""
        ]
