module Page.Browse exposing (..)

import Data exposing (Auth, Data)
import Data.Client exposing (Client)
import Data.Project as Project exposing (Part, PartId, ProcessId, Project, newPart)
import Data.User
import Data.Work as Work exposing (Work, WorkId)
import Dict
import Dict.Extra as Dict
import Firestore exposing (Lens)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path exposing (Id)
import Firestore.Remote as Remote
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Attribute, Html, div, i, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events
import Maybe.Extra as Maybe
import Set exposing (Set)
import Util exposing (classIf, flip, onDragEnter, onMouseDownStop)
import View.Button as Button


type alias Model =
    { selection : Set Id
    }


init : Model
init =
    { selection = Set.empty }


type Msg
    = AddPart (List ProcessId) PartId Part
    | CreatedWorkFolder ProcessId PartId GDrive.FileMeta
    | SelectWork Id Bool Bool
    | ClearSelection
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
                (Work.init folder.id folder.name processId partId)
            , Cmd.none
            )

        SelectWork workId select clear ->
            ( { model
                | selection =
                    (if select then
                        Set.insert

                     else
                        Set.remove
                    )
                        workId
                        (if clear then
                            Set.empty

                         else
                            model.selection
                        )
              }
            , Update.none
            , Cmd.none
            )

        ClearSelection ->
            ( { model | selection = Set.empty }
            , Update.none
            , Cmd.none
            )

        None ->
            ( model, Update.none, Cmd.none )


view :
    Auth
    -> Model
    -> Data
    -> Project
    -> Accessor Data (Html Msg)
view auth model data project =
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
            (o (Data.project project.id) <| o Project.works Lens.getAll)
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
                                                , ( partId, work )
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
            div
                [ style "min-height" "100vh"
                , style "width" "100%"
                , onMouseDownStop ClearSelection
                ]
                [ table
                    [ class "ui definition celled table select-none"
                    ]
                    [ thead []
                        [ tableHeader model processes
                        ]
                    , tbody [] <|
                        List.map
                            (\( partId, part ) ->
                                let
                                    selected =
                                        Set.member partId model.selection
                                in
                                tr [] <|
                                    td
                                        [ class "selectable"
                                        , classIf selected "active"
                                        , onMouseDownStop <|
                                            SelectWork partId
                                                (not selected)
                                                True
                                        , onDragEnter <|
                                            SelectWork partId
                                                (not selected)
                                                False
                                        ]
                                        [ text part.name ]
                                        :: List.map
                                            (\( processId, process ) ->
                                                Dict.get processId works
                                                    |> Maybe.andThen
                                                        (Dict.get partId)
                                                    |> Maybe.unwrap
                                                        emptyCell
                                                        (workCell model)
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
                , actions
                ]


actions =
    div
        [ class "ui three column grid"
        , style "position" "fixed"
        , style "width" "calc(100% - 210px)"
        , style "margin" "0"
        , style "bottom" "0"
        ]
        [ div [ class "column" ]
            [ div [ class "ui fluid card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ text "メンバー" ] ]
                ]
            ]
        , div [ class "column" ]
            [ div [ class "ui fluid card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ text "スケジュール" ] ]
                ]
            ]
        , div [ class "column" ]
            [ div [ class "ui fluid card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ text "アクティビティ" ] ]
                ]
            ]
        ]


tableHeader model processes =
    tr [] <|
        th [] []
            :: List.map
                (\( id, process ) ->
                    let
                        selected =
                            Set.member id model.selection
                    in
                    th
                        [ class "selectable"
                        , classIf selected "active"
                        , onMouseDownStop <| SelectWork id (not selected) True
                        , onDragEnter <| SelectWork id (not selected) False
                        ]
                        [ text process.name ]
                )
                processes


emptyCell : Html msg
emptyCell =
    td
        [ class "disabled" ]
        [ i [ class "plus icon" ]
            []
        ]


workCell : Model -> Work -> Html Msg
workCell model work =
    let
        selected =
            Set.member work.id model.selection
                || Set.member work.process model.selection
                || List.any (flip Set.member model.selection) work.belongsTo
    in
    td
        [ class "selectable center aligned"
        , classIf selected "active"
        , onMouseDownStop <| SelectWork work.id (not selected) True
        , onDragEnter <| SelectWork work.id (not selected) False
        ]
        [ i
            [ class <|
                Work.iconClass work.status
            ]
            []
        ]
