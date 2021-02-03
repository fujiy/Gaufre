module Page exposing (..)

import Browser exposing (Document)
import Data exposing (Auth, Data)
import Firestore exposing (Firestore)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Attribute, Html, a, div, i, map, span, text)
import Html.Attributes as Html exposing (class)
import Html.Events exposing (onClick)
import Page.Dashboard as Dashboard
import Page.Projects as Projects
import Util exposing (..)


type Page
    = Projects Projects.Model
    | Dashboard


type Msg
    = ProjectsMsg Projects.Msg
    | SignOut


init : Page
init =
    Projects Projects.List


update : Auth -> Msg -> Page -> ( Page, Updater (Firestore Data), Cmd Msg )
update auth msg page =
    case ( msg, page ) of
        ( ProjectsMsg m, Projects model ) ->
            let
                ( model_, upd, cmd ) =
                    Projects.update auth m model
            in
            ( Projects model_, Update.firestore upd, Cmd.map ProjectsMsg cmd )

        _ ->
            ( page, Update.none, Cmd.none )


view : Auth -> Page -> Data -> Accessor (Document Msg)
view auth page data =
    Access.map (Document "Gaufre") <|
        Access.list <|
            case page of
                Projects model ->
                    [ menubar auth data page
                    , Access.map (map ProjectsMsg) <| Projects.view auth model data
                    ]

                Dashboard ->
                    [ menubar auth data page ]


menubar : Auth -> Data -> Page -> Accessor (Html Msg)
menubar auth data page =
    let
        pg_ =
            { projects = False, dashboard = False }

        pg =
            case page of
                Projects _ ->
                    { pg_ | projects = True }

                Dashboard ->
                    { pg_ | dashboard = True }
    in
    flip Access.map (Access.doc data.users auth.uid) <|
        \user ->
            div []
                [ div [ class "ui secondary pointing menu" ]
                    [ a
                        [ class "item", classIf pg.projects "active" ]
                        [ text "Projects" ]
                    , a [ class "item", classIf pg.dashboard "active" ]
                        [ text "Dashboard" ]
                    , a [ class "item" ] [ text "Direct" ]
                    , a [ class "item" ] [ text "Manage" ]
                    , a [ class "item" ] [ text "Create" ]
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
