module Page exposing (..)

import Browser
import Data exposing (Auth, Data, project)
import Data.Client as Client
import Data.Project exposing (Project)
import Firestore
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, div, i, map, text)
import Html.Attributes as Html exposing (class, href, style)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Page.Browse as Browse
import Page.Dashboard as Dashboard
import Page.Projects as Projects
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
    | Browse Browse.Model


type Msg
    = ProjectsMsg Projects.Msg
    | DashboardMsg Dashboard.Msg
    | BrowseMsg Browse.Msg
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
                            (\_ _ w f -> Browse <| Browse.initWithWork w f)
                            (Url.string
                                </> Url.string
                                <?> Q.string "work"
                                <?> Q.string "folder"
                            )
                        , Url.map (Browse Browse.init) Url.top
                        ]
        ]
        |> flip Url.parse url
        |> Maybe.withDefault model


initialize : Auth -> Model -> Cmd Msg
initialize auth model =
    case model.page of
        Browse m ->
            Browse.initialize auth m
                |> Cmd.map BrowseMsg

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

        ( BrowseMsg msg, Browse m ) ->
            let
                ( m_, upd, cmd ) =
                    Browse.update auth msg { project = model.project } m
            in
            ( { model | page = Browse m_ }
            , upd
            , Cmd.map BrowseMsg cmd
            )

        _ ->
            ( model, Update.none, Cmd.none )


view : Auth -> Model -> Data -> Accessor Data (Browser.Document Msg)
view auth model data =
    let
        projectAndPaths =
            Access.access
                (o (Data.currentProject auth model.project) Lens.get)
                data
                |> Access.maybe
                |> Access.withPaths

        project =
            Access.map Tuple.second projectAndPaths

        projectId =
            Access.map
                (Tuple.first >> List.head >> Maybe.andThen Path.getLast)
                projectAndPaths
    in
    Access.map (Browser.Document "Gaufre") <|
        Access.list <|
            [ Access.andThen (sidemenu auth model data) project
            , Access.map
                (\html ->
                    div
                        [ class "pusher"
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

                    Browse m ->
                        Access.fromJust project
                            |> Access.andThen
                                (Browse.view auth m data)
                            |> Access.map (map BrowseMsg)
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

                Browse _ ->
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
                    [ i [ class "columns icon" ] []
                    , text "Dashboard"
                    ]
                , a [ class "item", classIf pg.browse "active", link "" ]
                    [ i [ class "th icon" ] []
                    , text "Browse"
                    ]
                ]



-- , div [ class "ui fluid popup bottom right transition hidden" ]
--     [ Button.primary SignOut "Sign Out" ]
