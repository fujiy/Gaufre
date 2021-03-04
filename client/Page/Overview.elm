module Page.Overview exposing (..)

import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Data.Project as Project exposing (Part, PartId, Process, ProcessId, Project, newPart, work)
import Data.User as User exposing (User)
import Data.Work as Work exposing (Work)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Firestore exposing (Id)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path
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
    { selection : Set Id }


init : Model
init =
    { selection = Set.empty }


type Msg
    = None
    | MoveToWork Process Part Work
    | SelectWork Work.Id Bool Bool
    | ClearSelection
    | AddPart (List ProcessId) PartId Part
    | CreatedWorkFolder ProcessId PartId FileMeta
    | SetWorkStaffs (List Work) (List User.Id)
    | SetWorkReviewers (List Work) (List User.Id)


update :
    Auth
    -> Msg
    -> { project : Int }
    -> Model
    -> ( Model, Updater Data, Cmd Msg )
update auth msg m model =
    let
        projectLens =
            Data.currentProject auth m.project
    in
    case msg of
        MoveToWork process part work ->
            ( model
            , Update.none
            , Nav.pushUrl auth.navKey <|
                Url.absolute
                    [ String.fromInt m.project, process.name, part.name ]
                    [ Url.string "work" work.id ]
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

        AddPart processes newId newPart ->
            ( model
            , Update.modify projectLens Project.desc <|
                \p -> { p | parts = Dict.insert newId newPart p.parts }
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

        SetWorkStaffs works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify (o projectLens <| Project.work w.id)
                            Work.desc
                        <|
                            \work -> { work | staffs = userRefs users }
            , Cmd.none
            )

        SetWorkReviewers works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify (o projectLens <| Project.work w.id)
                            Work.desc
                        <|
                            \work -> { work | reviewers = userRefs users }
            , Cmd.none
            )

        None ->
            ( model, Update.none, Cmd.none )


userRefs : List User.Id -> List User.Reference
userRefs =
    List.map <|
        \id ->
            Firestore.ref <|
                Path.fromIds [ "users", id ]


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
    flip2 Access.map2
        (Access.access
            (o (Data.project project.id) <| o Project.works Lens.getAll)
            data
        )
        (Access.access (o (Data.projectMembers project) Lens.gets) data)
    <|
        \works_ members ->
            let
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
                [ style "min-height" "100vh"
                , style "width" "100%"
                , onMouseDownStop ClearSelection
                ]
                [ table
                    [ class "ui definition celled table select-none"
                    ]
                    [ thead [] [ tableHeader model processes ]
                    , tbody [] <|
                        List.map (tableRow model processes works) parts
                            ++ [ newPartButton project ]
                    ]
                , actions members selection
                ]


tableHeader : Model -> List ( ProcessId, Process ) -> Html Msg
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


tableRow :
    Model
    -> List ( ProcessId, Process )
    -> Dict ProcessId (Dict PartId Work)
    -> ( PartId, Part )
    -> Html Msg
tableRow model processes works ( partId, part ) =
    let
        selected =
            Set.member partId model.selection
    in
    tr [] <|
        td
            [ class "selectable right aligned"
            , classIf selected "active"
            , style "padding" "5px"
            , onMouseDownStop <| SelectWork partId (not selected) True
            , onDragEnter <| SelectWork partId (not selected) False
            ]
            [ text part.name ]
            :: List.map
                (\( processId, process ) ->
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
        , onMouseDownStop <| SelectWork work.id (not selectedOnly) True
        , onDragEnter <| SelectWork work.id (not selected) False
        , onDoubleClick <| MoveToWork process part work
        ]
        [ icon <| Work.iconClass <| Work.getStatus work ]


actions : List User -> List Work -> Html Msg
actions members selection =
    let
        staffs =
            gatherList (.staffs >> List.map Firestore.getId) selection

        reviewers =
            gatherList (.reviewers >> List.map Firestore.getId) selection

        ( bottom, transition ) =
            if List.isEmpty selection then
                ( "-50vh", "" )

            else
                ( "0", "bottom 0.2s ease-out" )
    in
    div
        [ class "ui two column grid"
        , style "position" "fixed"
        , style "width" "calc(100% - 210px)"
        , style "margin" "0"
        , style "bottom" bottom
        , style "transition" transition
        , onMouseDownStop None
        ]
        [ div [ class "column" ]
            [ div [ class "ui fluid card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ text "メンバー" ] ]
                , div [ class "content" ]
                    [ Html.p []
                        [ text "担当："
                        , User.selectionList
                            members
                            (Dict.keys staffs.all)
                            (Dict.keys staffs.partially)
                            |> Html.map (SetWorkStaffs selection)
                        ]
                    , Html.p []
                        [ text "チェック："
                        , User.selectionList
                            members
                            (Dict.keys reviewers.all)
                            (Dict.keys reviewers.partially)
                            |> Html.map (SetWorkReviewers selection)
                        ]
                    ]
                ]
            ]
        , div [ class "column" ]
            [ div [ class "ui fluid card" ]
                [ div [ class "content" ]
                    [ div [ class "header" ] [ text "スケジュール" ] ]
                ]
            ]
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
                    (Dict.keys project.processes)
                    newId
                    newPart
            ]
        ]


isSelected : Set Id -> Work -> Bool
isSelected selection work =
    Set.member work.id selection
        || Set.member work.process selection
        || List.any (flip Set.member selection) work.belongsTo


gatherList :
    (Work -> List comparable)
    -> List Work
    ->
        { all : Dict comparable (List Work)
        , partially : Dict comparable (List Work)
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
