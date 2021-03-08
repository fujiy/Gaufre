module Page.Dashboard exposing (..)

import Data exposing (..)
import Firestore
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, div, span, text)
import Html.Attributes as Html exposing (class)
import Html.Events
import Util exposing (..)


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
