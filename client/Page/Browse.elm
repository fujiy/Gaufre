module Page.Browse exposing (..)

import Data exposing (Auth, Data)
import Data.Client exposing (Client)
import Data.Project exposing (Process, Project)
import Data.User exposing (User)
import Dict
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, a, button, div, i, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Util exposing (flip)
import View.Button as Button


type Model
    = Model


init : Model
init =
    Model


type Msg
    = AddPart


update : Auth -> Msg -> Model -> ( Model, Updater Data, Cmd Msg )
update auth msg model =
    ( model, Update.none, Cmd.none )


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    let
        processes =
            Dict.toList project.processes
                |> List.sortBy (\( _, p ) -> p.order)
    in
    Access.success <|
        table [ class "ui celed table" ]
            [ thead []
                [ tr [] <|
                    th [] []
                        :: (flip List.map processes <|
                                \( pid, process ) ->
                                    th [] [ text process.name ]
                           )
                ]
            , tbody []
                [ tr [] [ td [] [ Button.add AddPart ] ]
                ]
            ]
