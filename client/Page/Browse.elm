module Page.Browse exposing (..)

import Data exposing (Auth, Data)
import Data.Client exposing (Client)
import Data.Project as Project exposing (Part, PartId, ProcessId, Project, newPart)
import Data.User
import Data.Work as Work exposing (Work)
import Dict
import Dict.Extra as Dict
import Firestore exposing (Lens)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path exposing (Id)
import Firestore.Remote as Remote
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Html, div, i, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Util exposing (flip)
import View.Button as Button


type Model
    = Model


init : Model
init =
    Model


type Msg
    = AddPart (List ProcessId) PartId Part
    | CreatedWorkFolder ProcessId PartId GDrive.FileMeta
    | None


update :
    Auth
    -> Msg
    -> Model
    -> Lens Data Project.Document
    -> ( Model, Updater Data, Cmd Msg )
update auth msg model projectLens =
    case msg of
        AddPart processes newId newPart ->
            ( model
            , Update.modify projectLens Project.desc <|
                \p ->
                    { p | parts = Dict.insert newId newPart p.parts }
            , List.map
                (\processId ->
                    GDrive.createFolder auth.token
                        newPart.name
                        [ processId ]
                        |> Cmd.map
                            (Result.map (CreatedWorkFolder processId newId)
                                >> Result.withDefault None
                            )
                )
                processes
                |> Cmd.batch
            )

        CreatedWorkFolder processId partId folder ->
            ( model
            , Update.set
                (o projectLens <| Project.work folder.id)
                Work.desc
                (Work.init folder.name processId partId)
            , Cmd.none
            )

        None ->
            ( model, Update.none, Cmd.none )


view :
    Auth
    -> Model
    -> Data
    -> Lens Data Project.Document
    -> Project
    -> Accessor Data (Html Msg)
view auth model data projectLens project =
    let
        processes =
            Dict.toList project.processes
                |> List.sortBy (\( _, p ) -> p.order)

        parts =
            Dict.toList project.parts
                |> List.sortBy (\( _, p ) -> p.order)
    in
    flip Access.map
        (Access.access
            (o projectLens <| o Project.works Lens.getAll)
            data
        )
    <|
        \works_ ->
            let
                works =
                    List.concatMap
                        (\( workId, rw ) ->
                            Remote.toMaybe rw
                                |> Maybe.unwrap []
                                    (\work ->
                                        List.map
                                            (\partId ->
                                                ( work.process
                                                , ( partId, ( workId, work ) )
                                                )
                                            )
                                            work.belongsTo
                                    )
                        )
                        works_
                        |> Dict.groupBy Tuple.first
                        |> Dict.map
                            (\_ ws ->
                                List.map Tuple.second ws
                                    |> Dict.fromList
                            )
            in
            table [ class "ui celled table" ]
                [ thead []
                    [ tr [] <|
                        th [] []
                            :: List.map
                                (\( processId, process ) ->
                                    th [] [ text process.name ]
                                )
                                processes
                    ]
                , tbody [] <|
                    List.map
                        (\( partId, part ) ->
                            tr [] <|
                                td [] [ text part.name ]
                                    :: List.map
                                        (\( processId, process ) ->
                                            Dict.get processId works
                                                |> Maybe.andThen
                                                    (Dict.get partId)
                                                |> Maybe.unwrap
                                                    emptyCell
                                                    workCell
                                        )
                                        processes
                        )
                        parts
                        ++ [ let
                                ( newId, newPart ) =
                                    Project.newPart project
                             in
                             tr []
                                [ td []
                                    [ Button.add <|
                                        AddPart
                                            (Dict.keys project.processes)
                                            newId
                                            newPart
                                    ]
                                ]
                           ]
                ]


emptyCell : Html msg
emptyCell =
    td
        [ class "disabled" ]
        [ i [ class "plus icon" ]
            []
        ]


workCell : ( Id, Work ) -> Html Msg
workCell ( workId, work ) =
    td [] [ text work.name ]
