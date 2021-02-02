module Page exposing (..)

import Data exposing (Auth, Data)
import Firestore exposing (Firestore)
import Firestore.Html as FS
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


view : Auth -> Firestore Data -> Page -> Html Msg
view auth fs page =
    FS.firestore fs <|
        \data ->
            case page of
                Projects model ->
                    div []
                        [ menubar auth data page
                        , map ProjectsMsg <| Projects.view auth data model
                        ]

                Dashboard ->
                    div []
                        [ menubar auth data page ]


menubar : Auth -> Data -> Page -> Html Msg
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
    div []
        [ div [ class "ui secondary pointing menu" ]
            [ div [ class "header item" ] [ text "Gaufre" ]
            , a
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
                    [ FS.doc data.users auth.uid <|
                        \user -> text user.name
                    ]
                ]
            ]

        -- , div [ class "ui fluid popup bottom right transition hidden" ]
        --     [ Button.primary SignOut "Sign Out" ]
        ]
