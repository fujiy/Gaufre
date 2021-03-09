module Page exposing (..)

import Browser
import Data exposing (..)
import Data.Client as Client
import Data.Project as Project
import Data.User as User
import Firestore
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path.Id as Id exposing (Id)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, div, map, text)
import Html.Attributes as Html exposing (class, href, style)
import Maybe.Extra as Maybe
import Page.Dashboard as Dashboard
import Page.Members as Members
import Page.Overview as Overview
import Page.Projects as Projects
import Page.Work as Work
import Url exposing (Url)
import Url.Builder
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as Q
import Util exposing (..)


type alias Model =
    { page : Page
    , project : Int
    }


type Page
    = Projects Projects.Model
    | Dashboard Dashboard.Model
    | Overview Overview.Model
    | Work Work.Model
    | Members Members.Model


type Msg
    = ProjectsMsg Projects.Msg
    | DashboardMsg Dashboard.Msg
    | OverviewMsg (Id Project) Overview.Msg
    | WorkMsg (Id Project) Work.Msg
    | MembersMsg (Id Project) Members.Msg
    | SignOut


init : Url -> Model
init =
    urlChanged
        { page = Projects Projects.init
        , project = 0
        }


urlChanged : Model -> Url -> Model
urlChanged model url =
    Url.oneOf
        [ Url.map (Model (Projects Projects.init) 0) Url.top
        , Url.map (flip Model) <|
            Url.int
                </> Url.oneOf
                        [ Url.map (Dashboard Dashboard.init) <|
                            Url.s "dashboard"
                        , Url.map
                            (\_ _ mw mf ->
                                Maybe.map
                                    (\workId ->
                                        Work <|
                                            Work.init (Id.fromString workId) <|
                                                Maybe.withDefault workId mf
                                    )
                                    mw
                                    |> Maybe.withDefault
                                        (Overview Overview.init)
                            )
                            (Url.string
                                </> Url.string
                                <?> Q.string "work"
                                <?> Q.string "folder"
                            )
                        , Url.map (Overview Overview.init) Url.top
                        , Url.map (Members Members.init) <| Url.s "members"
                        ]
        ]
        |> flip Url.parse url
        |> Maybe.withDefault model


initialize : Auth -> Model -> Cmd Msg
initialize auth model =
    case model.page of
        Work m ->
            Work.initialize auth m |> Cmd.map (WorkMsg Id.null)

        _ ->
            Cmd.none


update : Auth -> Msg -> Model -> ( Model, Updater Data Msg )
update auth message model =
    case ( message, model.page ) of
        ( ProjectsMsg msg, Projects m ) ->
            let
                ( m_, upd ) =
                    Projects.update auth msg m
            in
            ( { model | page = Projects m_ }
            , Update.map ProjectsMsg upd
            )

        ( OverviewMsg projectId msg, Overview m ) ->
            let
                ( m_, upd ) =
                    Overview.update auth
                        msg
                        { project = model.project }
                        projectId
                        m
            in
            ( { model | page = Overview m_ }
            , Update.map (OverviewMsg projectId) upd
            )

        ( WorkMsg projectId msg, Work m ) ->
            let
                ( m_, upd ) =
                    Work.update auth msg projectId m
            in
            ( { model | page = Work m_ }
            , Update.map (WorkMsg projectId) upd
            )

        ( MembersMsg projectId msg, Members m ) ->
            let
                ( m_, upd ) =
                    Members.update auth msg projectId m
            in
            ( { model | page = Members m_ }
            , Update.map (MembersMsg projectId) upd
            )

        _ ->
            ( model, Update.none )


view : Auth -> Model -> Data -> Accessor Data (Browser.Document Msg)
view auth model data =
    let
        project =
            Access.access
                (o (Client.currentProject auth model.project) Lens.get)
                data
                |> Access.maybe
    in
    Access.map (Browser.Document "Gaufre") <|
        Access.list <|
            [ Access.andThen (sidemenu auth model data) project
            , Access.map
                (\html ->
                    div
                        [ class "pusher select-none"
                        , style "width" "calc(100% - 210px)"
                        , style "margin-left" "210px"
                        ]
                        [ html ]
                )
              <|
                case model.page of
                    Projects m ->
                        Projects.view auth m data
                            |> Access.map (map ProjectsMsg)

                    Dashboard m ->
                        Access.fromJust project
                            |> Access.andThen
                                (Dashboard.view auth m data)
                            |> Access.map (map DashboardMsg)

                    Overview m ->
                        Access.fromJust project
                            |> Access.andThen
                                (\p ->
                                    Overview.view auth m data p
                                        |> Access.map
                                            (map <| OverviewMsg <| Id.self p)
                                )

                    Work m ->
                        Access.fromJust project
                            |> Access.andThen
                                (\p ->
                                    Work.view auth m data p
                                        |> Access.map
                                            (map <| WorkMsg <| Id.self p)
                                )

                    Members m ->
                        Access.fromJust project
                            |> Access.andThen
                                (\p ->
                                    Members.view auth m data p
                                        |> Access.map
                                            (map <| MembersMsg <| Id.self p)
                                )
            ]


sidemenu : Auth -> Model -> Data -> Maybe Project -> Accessor Data (Html Msg)
sidemenu auth model data mproject =
    let
        pg_ =
            { projects = False
            , dashboard = False
            , browse = False
            , members = False
            }

        pg =
            case model.page of
                Projects _ ->
                    { pg_ | projects = True }

                Dashboard _ ->
                    { pg_ | dashboard = True }

                Overview _ ->
                    { pg_ | browse = True }

                Work _ ->
                    { pg_ | browse = True }

                Members _ ->
                    { pg_ | members = True }

        link p =
            href <| Url.Builder.absolute [ String.fromInt model.project, p ] []
    in
    flip Access.map
        (Access.access (o (User.me auth) Lens.get) data)
    <|
        \user ->
            div [ class "ui visible vertical inverted sidebar menu" ]
                [ a
                    [ class "item"
                    , classIf pg.projects "active"
                    , href "/"
                    ]
                    [ if pg.projects then
                        text "全てのプロジェクト"

                      else
                        text <| Maybe.unwrap "プロジェクトなし" .name mproject
                    ]
                , a
                    [ class "item"
                    , classIf pg.dashboard "active"
                    , link "dashboard"
                    ]
                    [ icon "columns", text "ダッシュボード" ]
                , a [ class "item", classIf pg.browse "active", link "" ]
                    [ icon "th", text "ビュー" ]
                , a
                    [ class "item"
                    , classIf pg.members "active"
                    , link "members"
                    ]
                    [ icon "users", text "メンバー" ]
                , a
                    [ class "item"
                    , link "me"
                    , style "position" "fixed"
                    , style "bottom" "0"
                    , style "width" "210px"
                    ]
                    [ User.avatar user
                    , case mproject of
                        Just project ->
                            case Project.myRole project auth of
                                Project.Owner ->
                                    div [ class "ui basic label" ]
                                        [ text "管理者" ]

                                Project.Admin ->
                                    div [ class "ui basic label" ]
                                        [ text "管理者" ]

                                _ ->
                                    text ""

                        _ ->
                            text ""
                    ]
                ]



-- , div [ class "ui fluid popup bottom right transition hidden" ]
--     [ Button.primary SignOut "Sign Out" ]
