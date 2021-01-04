module View.Menu exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


bar : List (Html msg) -> Html msg
bar =
    div
        [ class "ui secondary pointing menu" ]
