module Page.Dashboard exposing (..)

import Data exposing (Auth, Data)
import Firestore exposing (Firestore)
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes as Html exposing (class, href)
import Html.Events exposing (onClick)
import Util exposing (..)
import View.Button as Button
import View.Menu as Menu


type Msg
    = SignOut


view : Auth -> Data -> Html Msg
view auth data =
    div []
        []
