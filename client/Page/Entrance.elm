module Page.Entrance exposing (..)

import Element as El exposing (Element)
import Html
import Html.Attributes as Html
import Html.Events as Html
import View.Button as Button
import View.Text as Text

type Msg =
    SignIn

view : Element Msg
view =
    El.column
        [El.centerX, El.centerY, El.spacing 5]
        [ El.el [El.centerX] <| Text.h1 "Gaufre"
        , El.el [El.centerX] <| El.text "A project and data management tool"
        , El.el [El.centerX] <| El.text "for anime production"
        , El.el [El.centerX, El.padding 10]
            <| Button.primary SignIn "Sign In with Google" ]

