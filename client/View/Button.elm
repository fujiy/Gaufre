module View.Button exposing (..)

import Element as El exposing (Element)
import Html
import Html.Attributes as Html
import Html.Events as Html


primary : msg -> String -> Element msg
primary msg text =
    El.html <|
        Html.button
            [ Html.onClick msg
            , Html.class "ui primary button"
            ]
            [ Html.text text ]
