module View.Text exposing (..)

import Element as El exposing (Element)
import Html
import Html.Attributes as Html

h1 : String -> Element msg
h1 text =
    El.html <| Html.h1
        [Html.class "ui header"]
        [Html.text text]
