module Page exposing (..)

import Array
import Browser
import Data exposing (Auth, Data)
import Data.Client as Client exposing (Client)
import Data.Project exposing (Project)
import Data.User exposing (User)
import Firestore exposing (Firestore)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Attribute, Html, a, div, i, map, span, text)
import Html.Attributes as Html exposing (class, href)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Page.Create as Create
import Page.Dashboard as Dashboard
import Page.Direct as Direct
import Page.Manage as Manage
import Page.Projects as Projects
import Url exposing (Url)
import Url.Builder
import Url.Parser as Url exposing ((</>))
import Util exposing (..)


type alias Model =
    { page : Page
    , project : Int
    }


type Page
    = Projects Projects.Model
    | Dashboard Dashboard.Model
    | Create Create.Model
    | Direct Direct.Model
    | Manage Manage.Model


type Msg
    = ProjectsMsg Projects.Msg
    | DashboardMsg Dashboard.Msg
    | CreateMsg Create.Msg
    | DirectMsg Direct.Msg
    | ManageMsg Manage.Msg
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
                        [ Url.map (Dashboard Dashboard.init) Url.top
                        , Url.map (Create Create.init) <| Url.s "create"
                        , Url.map (Direct Direct.init) <| Url.s "direct"
                        , Url.map (Manage Manage.init) <| Url.s "manage"
                        ]
        ]
        |> flip Url.parse url
        |> Maybe.withDefault model


update : Auth -> Msg -> Model -> ( Model, Updater Data, Cmd Msg )
update auth msg model =
    case ( msg, model.page ) of
        ( ProjectsMsg m, Projects pm ) ->
            let
                ( m_, upd, cmd ) =
                    Projects.update auth m pm
            in
            ( { model | page = Projects m_ }
            , upd
            , Cmd.map ProjectsMsg cmd
            )

        _ ->
            ( model, Update.none, Cmd.none )


view : Auth -> Model -> Data -> Accessor Data (Browser.Document Msg)
view auth model data =
    let
        project =
            Access.access
                (o (Data.myClient auth) <|
                    o Lens.get <|
                        o Client.projects <|
                            Lens.atArray model.project
                )
                data
                |> Access.andThen (Lens.derefAndAccess Data.project data)
                |> Access.thenAccess Lens.get
                |> Access.maybe
    in
    Access.map (Browser.Document "Gaufre") <|
        Access.list <|
            Access.andThen (menubar auth model data) project
                :: (case model.page of
                        Projects m ->
                            [ Projects.view auth m data
                                |> Access.map (map ProjectsMsg)
                            ]

                        Dashboard m ->
                            [ Access.fromJust project
                                |> Access.andThen
                                    (Dashboard.view auth m data)
                                |> Access.map (map DashboardMsg)
                            ]

                        Create m ->
                            [ Access.fromJust project
                                |> Access.andThen
                                    (Create.view auth m data)
                                |> Access.map (map CreateMsg)
                            ]

                        Direct m ->
                            [ Access.fromJust project
                                |> Access.andThen
                                    (Direct.view auth m data)
                                |> Access.map (map DirectMsg)
                            ]

                        Manage m ->
                            [ Access.fromJust project
                                |> Access.andThen
                                    (Manage.view auth m data)
                                |> Access.map (map ManageMsg)
                            ]
                   )


menubar : Auth -> Model -> Data -> Maybe Project -> Accessor Data (Html Msg)
menubar auth model data mproject =
    let
        pg_ =
            { projects = False
            , dashboard = False
            , create = False
            , direct = False
            , manage = False
            }

        pg =
            case model.page of
                Projects _ ->
                    { pg_ | projects = True }

                Dashboard _ ->
                    { pg_ | dashboard = True }

                Create _ ->
                    { pg_ | create = True }

                Direct _ ->
                    { pg_ | direct = True }

                Manage _ ->
                    { pg_ | manage = True }

        link p =
            href <| Url.Builder.absolute [ String.fromInt model.project, p ] []
    in
    flip Access.map
        (Access.access (o (Data.me auth) Lens.get) data)
    <|
        \user ->
            div [ class "ui secondary pointing menu" ]
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
                    , link ""
                    ]
                    [ text "Dashboard" ]
                , a [ class "item", classIf pg.create "active", link "create" ]
                    [ text "Create" ]
                , a [ class "item", classIf pg.direct "active", link "direct" ]
                    [ text "Direct" ]
                , a [ class "item", classIf pg.manage "active", link "manage" ]
                    [ text "Manage" ]
                , div [ class "right menu" ]
                    [ a
                        [ class "item"
                        , onClick SignOut
                        ]
                        [ text user.name ]
                    ]
                ]



-- , div [ class "ui fluid popup bottom right transition hidden" ]
--     [ Button.primary SignOut "Sign Out" ]
