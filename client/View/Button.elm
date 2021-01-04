module View.Button exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


primary : msg -> String -> Html msg
primary msg text =
    button
        [ onClick msg
        , class "ui primary button"
        ]
        [ Html.text text ]
