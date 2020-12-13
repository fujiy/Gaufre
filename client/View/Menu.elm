module View.Menu exposing (..)

import Element as El exposing (Element)
import Html.Attributes as Html


bar : List (Element msg) -> Element msg
bar contents =
    El.row
        [ El.htmlAttribute <| Html.class "ui menu" ]
        contents
