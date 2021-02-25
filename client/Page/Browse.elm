module Page.Browse exposing (..)

import Data exposing (Auth, Data)
import Data.Client exposing (Client)
import Data.Project as Project exposing (Process, Project)
import Data.User exposing (User)
import Dict
import Firestore exposing (Lens)
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


update :
    Auth
    -> Msg
    -> Model
    -> Lens Data Project.Document
    -> ( Model, Updater Data, Cmd Msg )
update auth msg model projectLens =
    case msg of
        AddPart ->
            ( model
            , Update.modify projectLens Project.desc Project.addPart
            , Cmd.none
            )


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    let
        processes =
            Dict.toList project.processes
                |> List.sortBy (\( _, p ) -> p.order)

        parts =
            Dict.toList project.parts
                |> List.sortBy (\( _, p ) -> p.order)
    in
    Access.success <|
        table [ class "ui celed table" ]
            [ thead []
                [ tr [] <|
                    th [] []
                        :: (flip List.map processes <|
                                \( id, process ) ->
                                    th [] [ text process.name ]
                           )
                ]
            , tbody [] <|
                List.map
                    (\( id, part ) ->
                        tr [] [ td [] [ text part.name ] ]
                    )
                    parts
                    ++ [ tr [] [ td [] [ Button.add AddPart ] ] ]
            ]
