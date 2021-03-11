module Page.Entrance exposing (..)

import Html exposing (Html, br, div, text)
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
                        [ div [ class "center aligned header" ]
                            [ text "Gaufre" ]
                        , div [ class "center aligned description" ]
                            [ text "アニメ制作のための"
                            , br [] []
                            , text "プロジェクト・データ管理ツール"
                            ]
                        ]
                    , div
                        [ class "ui bottom attached primary button"
                        , onClick SignIn
                        ]
                        [ text "Googleアカウントでログイン" ]
                    ]
                ]
            ]
        ]
