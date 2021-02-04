module Page.Dashboard exposing (..)

import Data exposing (Auth, Data)
import Data.User exposing (Project(..), User)
import Firestore exposing (Firestore)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes as Html exposing (class, href)
import Html.Events exposing (onClick)
import Util exposing (..)
import View.Button as Button
import View.Menu as Menu


type Model
    = Model


init : Model
init =
    Model


type Msg
    = SignOut


update : Auth -> Msg -> Model -> ( Model, Updater Data, Cmd Msg )
update auth msg model =
    ( model, Update.none, Cmd.none )


view : Auth -> Model -> Data -> User -> Project -> Accessor (Html Msg)
view auth model data user (Project project) =
    Access.just <|
        div []
            [ text <| project.name ]
