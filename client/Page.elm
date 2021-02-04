module Page exposing (..)

import Array
import Browser
import Data exposing (Auth, Data)
import Data.User exposing (Project(..), User)
import Firestore exposing (Firestore)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Attribute, Html, a, div, i, map, span, text)
import Html.Attributes as Html exposing (class, href)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Page.Dashboard as Dashboard
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


type Msg
    = ProjectsMsg Projects.Msg
    | DashboardMsg Dashboard.Msg
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
                        ]
        ]
        |> flip Url.parse url
        |> Debug.log "PARSE"
        |> Maybe.withDefault model


update : Auth -> Msg -> Model -> ( Model, Updater (Firestore Data), Cmd Msg )
update auth msg model =
    case ( msg, model.page ) of
        ( ProjectsMsg m, Projects pm ) ->
            let
                ( m_, upd, cmd ) =
                    Projects.update auth m pm
            in
            ( { model | page = Projects m_ }
            , Update.firestore upd
            , Cmd.map ProjectsMsg cmd
            )

        _ ->
            ( model, Update.none, Cmd.none )


view : Auth -> Model -> Data -> Accessor (Browser.Document Msg)
view auth model data =
    let
        user =
            Access.doc data.users auth.uid

        project =
            Access.map (.projects >> Array.get model.project) user
                |> Access.andThen (Maybe.map Access.get >> Access.maybe)
    in
    Access.map (Browser.Document "Gaufre") <|
        Access.list <|
            Access.andThen2 (menubar auth model data) user project
                :: (case model.page of
                        Projects m ->
                            [ Access.andThen (Projects.view auth m data) user
                                |> Access.map (map ProjectsMsg)
                            ]

                        Dashboard m ->
                            [ Access.fromJust project
                                |> Access.andThen2 (Dashboard.view auth m data) user
                                |> Access.map (map DashboardMsg)
                            ]
                   )


menubar : Auth -> Model -> Data -> User -> Maybe Project -> Accessor (Html Msg)
menubar auth model data user mproject =
    let
        pg_ =
            { projects = False, dashboard = False }

        pg =
            case model.page of
                Projects _ ->
                    { pg_ | projects = True }

                Dashboard _ ->
                    { pg_ | dashboard = True }

        link p =
            href <| Url.Builder.absolute [ String.fromInt model.project, p ] []
    in
    Access.just <|
        div []
            [ div [ class "ui secondary pointing menu" ]
                [ a
                    [ class "item"
                    , classIf pg.projects "active"
                    , href "/"
                    ]
                    [ if pg.projects then
                        text "Projects"

                      else
                        text <|
                            Maybe.unwrap
                                "No Project"
                                (\(Project p) -> p.name)
                                mproject
                    ]
                , a
                    [ class "item"
                    , classIf pg.dashboard "active"
                    , link ""
                    ]
                    [ text "Dashboard" ]
                , a [ class "item", link "create" ] [ text "Create" ]
                , a [ class "item", link "direct" ] [ text "Direct" ]
                , a [ class "item", link "manage" ] [ text "Manage" ]
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
            ]
