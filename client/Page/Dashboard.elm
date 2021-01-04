module Page.Dashboard exposing (..)

import Data exposing (Auth, Data)
import Firestore exposing (Firestore)
import Firestore.Html as FS
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes as Html exposing (class, href)
import Html.Events exposing (onClick)
import Util exposing (..)
import View.Button as Button
import View.Menu as Menu


type Msg
    = SignOut


view : Auth -> Firestore Data -> Html Msg
view auth =
    flip FS.firestore <|
        \data ->
            div []
                [ div [ class "ui secondary pointing menu" ]
                    [ div [ class "header item" ] [ text "Gaufre" ]
                    , a [ class "item active" ] [ text "Dashboard" ]
                    , a [ class "item" ] [ text "Direct" ]
                    , a [ class "item" ] [ text "Manage" ]
                    , a [ class "item" ] [ text "Create" ]
                    , div [ class "right menu" ]
                        [ a
                            [ class "browse item"
                            , onClick SignOut
                            ]
                            [ FS.doc data.users auth.uid <|
                                \user -> Html.text user.name
                            ]
                        ]
                    ]
                , div [ class "ui fluid popup bottom right transition hidden" ]
                    [ Button.primary SignOut "Sign Out" ]
                ]
