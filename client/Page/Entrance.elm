module Page.Entrance exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as Html exposing (class, style)
import Html.Events as Html exposing (onClick)
import View.Button as Button


type Msg
    = SignIn


view : Html Msg
view =
    div
        [ class "ui grid middle aligned"
        , style "height" "100vh"
        , style "width" "100%"
        , style "margin" "0px"
        ]
        [ div [ class "row" ]
            [ div [ class "column" ]
                [ div
                    [ class "ui centered card" ]
                    [ div [ class "content" ]
                        [ div [ class "center aligned header" ] [ text "Gaufre" ]
                        , div [ class "center aligned description" ]
                            [ text """A project and data management tool for
                                    anime production""" ]
                        ]
                    , div
                        [ class "ui bottom attached primary button"
                        , onClick SignIn
                        ]
                        [ text "Sign In with Google" ]
                    ]
                ]
            ]
        ]
