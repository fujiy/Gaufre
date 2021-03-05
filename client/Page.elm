module Page exposing (..)

import Browser
import Data exposing (Auth, Data, project)
import Data.Project exposing (Project)
import Data.User as User
import Firestore
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, div, map, span, text)
import Html.Attributes as Html exposing (class, href, style)
import Maybe.Extra as Maybe
import Page.Dashboard as Dashboard
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


type Msg
    = ProjectsMsg Projects.Msg
    | DashboardMsg Dashboard.Msg
    | OverviewMsg Overview.Msg
    | WorkMsg Work.Msg
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
                                            Work.init workId <|
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
                        ]
        ]
        |> flip Url.parse url
        |> Maybe.withDefault model


initialize : Auth -> Model -> Cmd Msg
initialize auth model =
    case model.page of
        Work m ->
            Work.initialize auth m
                |> Cmd.map WorkMsg

        _ ->
            Cmd.none


update : Auth -> Msg -> Model -> ( Model, Updater Data, Cmd Msg )
update auth message model =
    case ( message, model.page ) of
        ( ProjectsMsg msg, Projects m ) ->
            let
                ( m_, upd, cmd ) =
                    Projects.update auth msg m
            in
            ( { model | page = Projects m_ }
            , upd
            , Cmd.map ProjectsMsg cmd
            )

        ( OverviewMsg msg, Overview m ) ->
            let
                ( m_, upd, cmd ) =
                    Overview.update auth msg { project = model.project } m
            in
            ( { model | page = Overview m_ }
            , upd
            , Cmd.map OverviewMsg cmd
            )

        ( WorkMsg msg, Work m ) ->
            let
                ( m_, upd, cmd ) =
                    Work.update auth msg { project = model.project } m
            in
            ( { model | page = Work m_ }
            , upd
            , Cmd.map WorkMsg cmd
            )

        _ ->
            ( model, Update.none, Cmd.none )


view : Auth -> Model -> Data -> Accessor Data (Browser.Document Msg)
view auth model data =
    let
        project =
            Access.access
                (o (Data.currentProject auth model.project) Lens.get)
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
                                (Overview.view auth m data)
                            |> Access.map (map OverviewMsg)

                    Work m ->
                        Access.fromJust project
                            |> Access.andThen
                                (Work.view auth m data)
                            |> Access.map (map WorkMsg)
            ]


sidemenu : Auth -> Model -> Data -> Maybe Project -> Accessor Data (Html Msg)
sidemenu auth model data mproject =
    let
        pg_ =
            { projects = False
            , dashboard = False
            , browse = False
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

        link p =
            href <| Url.Builder.absolute [ String.fromInt model.project, p ] []
    in
    flip Access.map
        (Access.access (o (Data.me auth) Lens.get) data)
    <|
        \user ->
            div [ class "ui visible vertical inverted sidebar menu" ]
                [ a
                    [ class "item"
                    , classIf pg.projects "active"
                    , href "/"
                    ]
                    [ if pg.projects then
                        text "Projects"

                      else
                        text <| Maybe.unwrap "No Project" .name mproject
                    ]
                , a
                    [ class "item"
                    , classIf pg.dashboard "active"
                    , link "dashboard"
                    ]
                    [ icon "columns", text "Dashboard" ]
                , a [ class "item", classIf pg.browse "active", link "" ]
                    [ icon "th", text "Overview" ]
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
                            when (Data.isAdmin auth project) <|
                                div [ class "ui basic label" ]
                                    [ text "管理者" ]

                        _ ->
                            text ""
                    ]
                ]



-- , div [ class "ui fluid popup bottom right transition hidden" ]
--     [ Button.primary SignOut "Sign Out" ]
