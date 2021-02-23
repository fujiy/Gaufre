module Page.Create exposing (..)

import Data exposing (Auth, Data)
import Data.Client exposing (Client)
import Data.Project exposing (Project)
import Data.User exposing (User)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, div, i, span, text)


type Model
    = Model


init : Model
init =
    Model


type Msg
    = Msg


update : Auth -> Msg -> Model -> ( Model, Updater Data, Cmd Msg )
update auth msg model =
    ( model, Update.none, Cmd.none )


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    Access.success <|
        div []
            [ text <| project.name ]
