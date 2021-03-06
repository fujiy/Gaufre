module Page.Overview exposing (..)

import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Data.Project as Project exposing (Part, Process, Project, newPart, work)
import Data.User as User exposing (User)
import Data.Work as Work exposing (Work)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Firestore exposing (Id)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path exposing (Id(..), SomeId, unId)
import Firestore.Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive exposing (FileMeta)
import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onDoubleClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Set exposing (Set)
import Url.Builder as Url
import Util exposing (..)
import View.Button as Button


type alias Model =
    { selection : Set SomeId }


init : Model
init =
    { selection = Set.empty }


type Msg
    = None
    | MoveToWork Process Part Work
    | SelectWork (Id Work) Bool Bool
    | ClearSelection
    | AddPart (List (Id Process)) (Id Part) Part
    | CreatedWorkFolder (Id Process) (Id Part) FileMeta
    | SetWorkStaffs (List Work) (List (Id User))
    | SetWorkReviewers (List Work) (List (Id User))


update :
    Auth
    -> Msg
    -> { project : Int }
    -> Model
    -> ( Model, Updater Data Msg )
update auth msg m model =
    let
        projectLens =
            Data.currentProject auth m.project
    in
    case msg of
        MoveToWork process part work ->
            ( model
            , Update.command <|
                \_ ->
                    Nav.pushUrl auth.navKey <|
                        Url.absolute
                            [ String.fromInt m.project, process.name, part.name ]
                            [ Url.string "work" work.id ]
            )

        SelectWork (Id workId) select clear ->
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
            )

        ClearSelection ->
            ( { model | selection = Set.empty }
            , Update.none
            )

        AddPart processes (Id newId) newPart ->
            ( model
            , Update.all
                [ Update.modify projectLens Project.desc <|
                    \p -> { p | parts = Dict.insert newId newPart p.parts }
                , List.map
                    (\(Id processId) _ ->
                        GDrive.createFolder auth.token
                            newPart.name
                            [ processId ]
                            |> Cmd.map
                                (Result.map
                                    (CreatedWorkFolder
                                        (Id processId)
                                        (Id newId)
                                    )
                                    >> Result.withDefault None
                                )
                    )
                    processes
                    |> Update.batch
                ]
            )

        CreatedWorkFolder processId partId folder ->
            ( model
            , Update.set
                (o projectLens <| Project.work <| Id folder.id)
                Work.desc
                (Work.init (Id folder.id) folder.name processId partId)
            )

        SetWorkStaffs works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify (o projectLens <| Project.work <| Id w.id)
                            Work.desc
                        <|
                            \work -> { work | staffs = userRefs users }
            )

        SetWorkReviewers works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify (o projectLens <| Project.work <| Id w.id)
                            Work.desc
                        <|
                            \work -> { work | reviewers = userRefs users }
            )

        None ->
            ( model, Update.none )


userRefs : List (Id User) -> List User.Reference
userRefs =
    List.map <|
        \(Id id) ->
            Firestore.ref <|
                Path.fromIds [ "users", id ]


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    let
        processes =
            Dict.toList project.processes
                |> List.sortBy (\( _, p ) -> p.order)
                |> List.map (Tuple.mapFirst Id)

        parts =
            Dict.toList project.parts
                |> List.sortBy (\( _, p ) -> p.order)
                |> List.map (Tuple.mapFirst Id)
    in
    flip2 Access.map2
        (Access.access
            (o (Data.project <| Id project.id) <| o Project.works Lens.getAll)
            data
        )
        (Access.access (o (Data.projectMembers project) Lens.gets) data)
    <|
        \works_ members ->
            let
                role =
                    Data.myRole project auth

                works =
                    List.concatMap
                        (\work ->
                            List.map
                                (\partId ->
                                    ( work.process
                                    , ( partId, work )
                                    )
                                )
                                work.belongsTo
                        )
                        works_
                        |> Dict.groupBy Tuple.first
                        |> Dict.map
                            (\_ ws ->
                                List.map Tuple.second ws
                                    |> Dict.fromList
                            )

                selection =
                    List.filter (isSelected model.selection) works_
            in
            div
                [ --  style "min-height" "100vh"
                  -- , style "width" "100%"
                  class "ui grid"
                , onMouseDownStop ClearSelection
                ]
                [ table
                    [ class "ui ten wide column definition celled table select-none"
                    ]
                    [ thead [] [ tableHeader model processes ]
                    , tbody [] <|
                        List.map (tableRow model processes works) parts
                            ++ (if Project.authority role |> .editStructure then
                                    [ newPartButton project ]

                                else
                                    []
                               )
                    ]
                , actions role members selection
                ]


tableHeader : Model -> List ( Id Process, Process ) -> Html Msg
tableHeader model processes =
    tr [] <|
        th [] []
            :: List.map
                (\( Id id, process ) ->
                    let
                        selected =
                            Set.member id model.selection
                    in
                    th
                        [ class "selectable"
                        , classIf selected "active"
                        , onMouseDownStop <|
                            SelectWork (Id id) (not selected) True
                        , onDragEnter <|
                            SelectWork (Id id) (not selected) False
                        ]
                        [ text process.name ]
                )
                processes


tableRow :
    Model
    -> List ( Id Process, Process )
    -> Dict SomeId (Dict SomeId Work)
    -> ( Id Part, Part )
    -> Html Msg
tableRow model processes works ( Id partId, part ) =
    let
        selected =
            Set.member partId model.selection
    in
    tr [] <|
        td
            [ class "selectable right aligned"
            , classIf selected "active"
            , style "padding" "5px"
            , onMouseDownStop <| SelectWork (Id partId) (not selected) True
            , onDragEnter <| SelectWork (Id partId) (not selected) False
            ]
            [ text part.name ]
            :: List.map
                (\( Id processId, process ) ->
                    Dict.get processId works
                        |> Maybe.andThen (Dict.get partId)
                        |> Maybe.unwrap emptyCell (workCell model process part)
                )
                processes


emptyCell : Html msg
emptyCell =
    td
        [ class "disabled" ]
        [ icon "plus" ]


workCell : Model -> Process -> Part -> Work -> Html Msg
workCell model process part work =
    let
        selected =
            isSelected model.selection work

        selectedOnly =
            model.selection == Set.singleton work.id
    in
    td
        [ class "selectable center aligned"
        , classIf selected "active"
        , onMouseDownStop <| SelectWork (Id work.id) (not selectedOnly) True
        , onDragEnter <| SelectWork (Id work.id) (not selected) False
        , onDoubleClick <| MoveToWork process part work
        ]
        [ icon <| Work.iconClass <| Work.getStatus work ]


actions : Project.Role -> List User -> List Work -> Html Msg
actions role members selection =
    let
        staffs =
            gatherList (.staffs >> List.map (Firestore.getId >> unId))
                selection

        reviewers =
            gatherList (.reviewers >> List.map (Firestore.getId >> unId))
                selection

        status =
            List.map Work.getStatus selection
                |> List.sortBy Work.statusNumber
                |> List.uniqueBy Work.statusNumber
    in
    div
        [ class "ui six wide column grid card"

        -- , style "position" "fixed"
        -- , style "width" "calc(100% - 210px)"
        -- , style "margin" "0"
        -- , style "bottom" bottom
        -- , style "transition" transition
        , onMouseDownStop None
        ]
    <|
        if List.isEmpty selection then
            []

        else
            [ div [ class "content" ]
                [ div [ class "header" ]
                    [ case selection of
                        [ work ] ->
                            text work.name

                        _ ->
                            text <|
                                String.fromInt (List.length selection)
                                    ++ "件の作業"
                    , div [ class "meta" ] []
                    , div [ class "description" ] <|
                        List.map Work.statusLabel status
                    ]
                ]
            , div [ class "content" ]
                [ div [ class "header" ] [ text "メンバー" ]
                , Html.p []
                    [ text "担当："
                    , User.list (Project.authority role |> .manageWorkStaffs)
                        members
                        (Dict.keys staffs.all |> List.map Id)
                        (Dict.keys staffs.partially |> List.map Id)
                        |> Html.map (SetWorkStaffs selection)
                    ]
                , Html.p []
                    [ text "チェック："
                    , User.list (Project.authority role |> .manageWorkStaffs)
                        members
                        (Dict.keys reviewers.all |> List.map Id)
                        (Dict.keys reviewers.partially |> List.map Id)
                        |> Html.map (SetWorkReviewers selection)
                    ]
                ]
            , div [ class "content" ]
                [ div [ class "header" ] [ text "スケジュール" ] ]
            ]


newPartButton project =
    let
        ( newId, newPart ) =
            Project.newPart project
    in
    tr []
        [ td []
            [ Button.add <|
                AddPart
                    (Dict.keys project.processes |> List.map Id)
                    newId
                    newPart
            ]
        ]


isSelected : Set SomeId -> Work -> Bool
isSelected selection work =
    Set.member work.id selection
        || Set.member work.process selection
        || List.any (flip Set.member selection) work.belongsTo


gatherList :
    (a -> List comparable)
    -> List a
    ->
        { all : Dict comparable (List a)
        , partially : Dict comparable (List a)
        }
gatherList getter =
    List.foldr
        (\work result ->
            let
                items =
                    getter work
                        |> List.map (\item -> ( item, [ work ] ))
                        |> Dict.fromList
            in
            if result.notAll then
                { result
                    | partially =
                        Dict.merge Dict.insert
                            (\item xs ys -> Dict.insert item <| xs ++ ys)
                            Dict.insert
                            items
                            result.partially
                            Dict.empty
                }

            else if Dict.isEmpty items then
                { result
                    | notAll = True
                    , all = Dict.empty
                    , partially =
                        Dict.merge Dict.insert
                            (\item xs ys -> Dict.insert item <| xs ++ ys)
                            Dict.insert
                            result.all
                            result.partially
                            Dict.empty
                }

            else if
                Dict.isEmpty result.all
                    && Dict.isEmpty result.partially
                    && not result.notAll
            then
                { result | all = items }

            else
                Dict.merge
                    (\item _ rs ->
                        { rs
                            | all = Dict.remove item rs.all
                            , partially =
                                Dict.update item
                                    (Maybe.unwrap [ work ] ((::) work) >> Just)
                                    rs.partially
                        }
                    )
                    (\item _ _ rs ->
                        { rs
                            | all =
                                Dict.update item (Maybe.map <| (::) work) rs.all
                        }
                    )
                    (\item _ rs ->
                        { rs
                            | partially =
                                Dict.update item
                                    (Maybe.unwrap [ work ] ((::) work) >> Just)
                                    rs.partially
                        }
                    )
                    result.all
                    items
                    result
        )
        { all = Dict.empty, partially = Dict.empty, notAll = False }
        >> (\r -> { all = r.all, partially = r.partially })
