module Page.Dashboard exposing (..)

import Element as El exposing (Element)
import View.Menu as Menu
import View.Text as Text


type Msg
    = SignOut


view : Element Msg
view =
    Menu.bar
        [ Text.h1 "Gaufre"
        ]
