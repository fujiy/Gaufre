module Page.Dashboard exposing (..)

import Data exposing (Auth, Data)
import Data.Client exposing (Client)
import Data.Project exposing (Project)
import Data.User exposing (User)
import Firestore exposing (Firestore)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes as Html exposing (class, href)
import Html.Events exposing (onClick)
import Util exposing (..)
import View.Button as Button


type Model
    = Model


init : Model
init =
    Model


type Msg
    = SignOut


update : Auth -> Msg -> Model -> ( Model, Updater Data Msg )
update auth msg model =
    ( model, Update.none )


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    Access.success <|
        div []
            [ text <| project.name ]
