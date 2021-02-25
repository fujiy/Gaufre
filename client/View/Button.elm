module View.Button exposing (..)

import Html exposing (Html, button, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


primary : msg -> String -> Html msg
primary msg txt =
    button
        [ onClick msg
        , class "ui primary button"
        ]
        [ text txt ]


add : msg -> Html msg
add msg =
    button
        [ class "ui icon button"
        , onClick msg
        ]
        [ i [ class "plus icon" ] [] ]
