module Page.Dashboard exposing (..)

import Data exposing (Auth, Data)
import Element as El exposing (Element)
import Firestore exposing (Firestore)
import Firestore.Element as El
import Util exposing (..)
import View.Menu as Menu
import View.Text as Text


type Msg
    = SignOut


view : Auth -> Firestore Data -> Element Msg
view auth =
    flip El.firestore <|
        \data ->
            Menu.bar
                [ Text.h1 "Gaufre"
                , El.doc data.users auth.uid <|
                    \user ->
                        Text.h1 user.name
                ]
