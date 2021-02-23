module Page.Manage exposing (..)

import Data exposing (Auth, Data)
import Data.Client exposing (Client)
import Data.Project exposing (Project)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes as Html exposing (class, href)


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
        div [ class "ui vertical menu" ]
            [ a [ class "item" ] [ text "進行" ]
            , a [ class "item" ] [ text "メンバー" ]
            , a [ class "item" ] [ text "データ" ]
            , a [ class "item" ] [ text "設定" ]
            ]
