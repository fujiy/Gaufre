module Page.Overview exposing (..)

import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Data.Project as Project exposing (Project, newPart, work)
import Data.User as User exposing (User)
import Data.Work as Work exposing (Part, Process, Work)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Firestore exposing (Id)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id(..), IdMap, SomeId, unId)
import Firestore.Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive exposing (FileMeta)
import Html exposing (Html, button, div, input, label, node, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, colspan, placeholder, rowspan, style, type_, value)
import Html.Events exposing (onCheck, onClick, onDoubleClick, onInput)
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Set exposing (Set)
import Url.Builder as Url
import Util exposing (..)
import View.Button as Button


type alias Model =
    { selection : Set SomeId
    , modal : Modal
    }


type Modal
    = Hidden
    | AddPartModal (IdMap Process ( Process, Bool )) (Id Part) Part
    | AddWorkModal (Id Process) (Id Part) String


init : Model
init =
    { selection = Set.empty, modal = Hidden }


type Msg
    = None
    | ModalState Modal
    | MoveToWork Process Part Work
    | Select SomeId Bool Bool
    | ClearSelection
    | AddPart (List (Id Process)) (Id Part) Part
    | AddWork (Id Process) (Id Part) String
    | CreatedWorkFolder (Id Process) (Id Part) FileMeta
    | SetWorkStaffs (List Work) (List (Id User))
    | SetWorkReviewers (List Work) (List (Id User))
    | SetWorkBelongsTo Work (List (Id Part))


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
        ModalState modal ->
            ( { model | modal = modal }
            , Update.none
            )

        MoveToWork process part work ->
            ( model
            , Update.command <|
                \_ ->
                    Nav.pushUrl auth.navKey <|
                        Url.absolute
                            [ String.fromInt m.project
                            , process.name
                            , part.name
                            ]
                            [ Url.string "work" work.id ]
            )

        Select workId select clear ->
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

        AddPart processes newId newPart ->
            ( model
            , Update.all
                [ Update.modify projectLens Project.desc <|
                    \p -> { p | parts = Id.insert newId newPart p.parts }
                , List.map
                    (\processId _ ->
                        GDrive.createFolder auth.token
                            newPart.name
                            [ unId processId ]
                            |> Cmd.map
                                (Result.map
                                    (CreatedWorkFolder processId newId)
                                    >> Result.withDefault None
                                )
                    )
                    processes
                    |> Update.batch
                ]
            )

        AddWork processId partId name ->
            ( model
            , Update.command <|
                \_ ->
                    GDrive.createFolder auth.token
                        name
                        [ unId processId ]
                        |> Cmd.map
                            (Result.map
                                (CreatedWorkFolder processId partId)
                                >> Result.withDefault None
                            )
            )

        CreatedWorkFolder processId partId folder ->
            ( model
            , Update.set
                (o projectLens <| Project.work <| Id.fromString folder.id)
                Work.desc
                (Work.init (Id.fromString folder.id)
                    folder.name
                    processId
                    partId
                )
            )

        SetWorkStaffs works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify
                            (o projectLens <| Project.work <| Id.self w)
                            Work.desc
                        <|
                            \work -> { work | staffs = userRefs users }
            )

        SetWorkReviewers works users ->
            ( model
            , Update.all <|
                flip List.map works <|
                    \w ->
                        Update.modify
                            (o projectLens <| Project.work <| Id.self w)
                            Work.desc
                        <|
                            \work -> { work | reviewers = userRefs users }
            )

        SetWorkBelongsTo w parts ->
            ( model
            , Update.modify
                (o projectLens <| Project.work <| Id.self w)
                Work.desc
              <|
                \work -> { work | belongsTo = parts }
            )

        None ->
            ( model, Update.none )


userRefs : List (Id User) -> List User.Reference
userRefs =
    List.map <|
        \id ->
            Firestore.ref <|
                Path.fromIds [ "users", unId id ]


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    let
        processes =
            Id.toList project.processes
                |> List.sortBy (\( _, p ) -> p.order)

        parts =
            Id.toList project.parts
                |> List.sortBy (\( _, p ) -> p.order)
    in
    flip2 Access.map2
        (Access.access
            (o (Data.project <| Id.self project) <| o Project.works Lens.getAll)
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
                                (\partId -> ( work.process, ( partId, work ) ))
                                work.belongsTo
                        )
                        works_
                        |> Id.groupBy Tuple.first
                        |> Id.map (List.map Tuple.second >> Id.fromList)

                partIds =
                    List.map Tuple.first parts
            in
            div
                [ class "ui grid"
                , style "width" "calc(100vw - 210px)"
                , style "height" "100vh"
                , style "margin" "0"
                ]
                [ table
                    [ class "ui ten wide column definition celled table"
                    , class "single line structured select-none"
                    , style "overflow" "scroll"
                    , style "height" "100vh"
                    , onMouseDownStop ClearSelection
                    ]
                    [ thead [] [ tableHeader model processes ]
                    , tbody [] <|
                        List.map
                            (tableRow auth
                                model
                                project
                                partIds
                                processes
                                works
                            )
                            parts
                            ++ (if Project.authority role |> .editStructure then
                                    [ tr []
                                        [ td
                                            [ colspan <| List.length processes ]
                                            [ Button.add <| addPart project ]
                                        ]
                                    ]

                                else
                                    []
                               )
                    ]
                , actions model project role members works works_
                , modalView model
                ]


tableHeader : Model -> List ( Id Process, Process ) -> Html Msg
tableHeader model processes =
    tr [] <|
        th [] []
            :: List.map
                (\( id, process ) ->
                    let
                        selected =
                            Set.member (unId id) model.selection
                    in
                    th
                        [ class "selectable collapsing"
                        , classIf selected "active"
                        , style "cursor" "pointer"
                        , onMouseDownStop <|
                            Select (unId id) (not selected) True
                        , onDragEnter <|
                            Select (unId id) (not selected) False
                        ]
                        [ text process.name ]
                )
                processes


tableRow :
    Auth
    -> Model
    -> Project
    -> List (Id Part)
    -> List ( Id Process, Process )
    -> IdMap Process (IdMap Part Work)
    -> ( Id Part, Part )
    -> Html Msg
tableRow auth model project partIds processes works ( partId, part ) =
    let
        selected =
            Set.member (unId partId) model.selection
    in
    tr [] <|
        td
            [ class "selectable right aligned"
            , classIf selected "active"
            , style "padding" "5px"
            , style "cursor" "pointer"
            , onMouseDownStop <| Select (unId partId) (not selected) True
            , onDragEnter <| Select (unId partId) (not selected) False
            ]
            [ text part.name ]
            :: List.map
                (\( processId, process ) ->
                    let
                        parts =
                            Id.get processId works
                                |> Maybe.withDefault Id.empty
                    in
                    Id.get partId parts
                        |> Maybe.unwrap
                            (emptyCell (Data.myRole project auth)
                                processId
                                partId
                                part
                            )
                            (workCell model partIds process partId part)
                )
                processes


emptyCell : Project.Role -> Id Process -> Id Part -> Part -> Html Msg
emptyCell role processId partId part =
    td
        [ class "center aligned", style "padding" "0" ]
        [ when (Project.authority role |> .editStructure) <|
            button
                [ class "ui compact basic icon button"
                , style "box-shadow" "none"
                , onClick <|
                    ModalState <|
                        AddWorkModal processId partId part.name
                ]
                [ icon "plus" ]
        ]


workCell :
    Model
    -> List (Id Part)
    -> Process
    -> Id Part
    -> Part
    -> Work
    -> Html Msg
workCell model partIds process partId part work =
    let
        selected =
            isSelected model.selection work

        selectedOnly =
            model.selection == Set.singleton work.id
    in
    case partRowSpan partIds partId work of
        0 ->
            text ""

        n ->
            td
                [ class "center aligned"
                , classIf selected "active"
                , style "cursor" "pointer"
                , rowspan n
                , onMouseDownStop <| Select work.id (not selectedOnly) True
                , onDragEnter <| Select work.id (not selected) False
                , onDoubleClick <| MoveToWork process part work
                ]
                [ icon <| Work.iconClass <| Work.getStatus work ]


partRowSpan : List (Id Part) -> Id Part -> Work -> Int
partRowSpan ps partId work =
    let
        workParts =
            List.map unId work.belongsTo
                |> Set.fromList

        goBefore parts =
            case parts of
                [] ->
                    0

                id :: parts_ ->
                    if Set.member (unId id) workParts then
                        if id == partId then
                            goAfter parts_

                        else
                            goSkip parts_

                    else
                        goBefore parts_

        goSkip parts =
            case parts of
                [] ->
                    0

                id :: parts_ ->
                    if id == partId then
                        0

                    else if Set.member (unId id) workParts then
                        goSkip parts_

                    else
                        goBefore parts_

        goAfter parts =
            case parts of
                [] ->
                    1

                id :: parts_ ->
                    if Set.member (unId id) workParts then
                        goAfter parts_ + 1

                    else
                        1
    in
    goBefore ps


actions :
    Model
    -> Project
    -> Project.Role
    -> List User
    -> IdMap Process (IdMap Part Work)
    -> List Work
    -> Html Msg
actions model project role members workMap works =
    let
        selection =
            List.filter (isSelected model.selection) works

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

        authority =
            Project.authority role
    in
    div
        [ class "ui six wide column grid card"
        , style "margin" "0"
        , style "height" "100vh"
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
                    , User.list authority.manageWorkStaffs
                        members
                        (Dict.keys staffs.all |> List.map Id)
                        (Dict.keys staffs.partially |> List.map Id)
                        |> Html.map (SetWorkStaffs selection)
                    ]
                , Html.p []
                    [ text "チェック："
                    , User.list authority.manageWorkStaffs
                        members
                        (Dict.keys reviewers.all |> List.map Id)
                        (Dict.keys reviewers.partially |> List.map Id)
                        |> Html.map (SetWorkReviewers selection)
                    ]
                ]
            , div [ class "content" ]
                [ div [ class "header" ] [ text "スケジュール" ] ]
            , when authority.editStructure <|
                div [ class "content" ]
                    [ div [ class "header" ] [ text "設定" ]
                    , Html.p [] <|
                        case selection of
                            [ work ] ->
                                let
                                    parts =
                                        Id.get work.process workMap
                                            |> Maybe.withDefault Id.empty

                                    choices =
                                        Id.filter
                                            (\id _ ->
                                                not (Id.member id parts)
                                                    || List.member id
                                                        work.belongsTo
                                            )
                                            project.parts
                                in
                                [ text "含むカット："
                                , Work.partList choices work
                                    |> Html.map (SetWorkBelongsTo work)
                                ]

                            _ ->
                                []
                    ]
            ]


addPart : Project -> Msg
addPart project =
    let
        ( newId, newPart ) =
            Project.newPart project

        processes =
            flip Id.map project.processes <| \process -> ( process, True )
    in
    ModalState <| AddPartModal processes newId newPart


modalView : Model -> Html Msg
modalView model =
    node "ui-modal"
        [ class "ui small modal"
        , boolAttr "show" <| model.modal /= Hidden
        , onHide <| ModalState Hidden
        , onApprove <|
            case model.modal of
                AddPartModal processes newId newPart ->
                    AddPart
                        (Id.toList processes
                            |> List.filterMap
                                (\( id, ( _, add ) ) ->
                                    if add then
                                        Just id

                                    else
                                        Nothing
                                )
                        )
                        newId
                        newPart

                AddWorkModal processId partId name ->
                    AddWork processId partId name

                Hidden ->
                    None
        , attribute "not-hide" "true"
        ]
    <|
        case model.modal of
            AddPartModal processes id part ->
                let
                    processList =
                        Id.toList processes
                            |> List.sortBy (\( _, ( p, _ ) ) -> p.order)

                    checkProcess processId check =
                        ModalState <|
                            AddPartModal
                                (Id.modify processId
                                    (\( p, _ ) -> ( p, check ))
                                    processes
                                )
                                id
                                part
                in
                [ div [ class "header" ] [ text "カットを追加" ]
                , div [ class "content" ]
                    [ Html.p []
                        [ text "名前："
                        , div [ class "ui input select-all" ]
                            [ input
                                [ type_ "text"
                                , placeholder "名前を入力"
                                , onInput <|
                                    \name ->
                                        ModalState <|
                                            AddPartModal processes
                                                id
                                                { part | name = name }
                                , value part.name
                                ]
                                []
                            ]
                        ]
                    , div [ class "ui horizontal list" ] <|
                        flip List.map processList <|
                            \( pid, ( process, add ) ) ->
                                div [ class "item" ]
                                    [ div [ class "ui checkbox" ]
                                        [ input
                                            [ type_ "checkbox"
                                            , checked add
                                            , onCheck <| checkProcess pid
                                            ]
                                            []
                                        , label [] [ text process.name ]
                                        ]
                                    ]
                    ]
                , div [ class "actions" ]
                    [ div [ class "ui deny button" ] [ text "完了" ]
                    , div
                        [ class "ui positive button"
                        , classIf (part.name == "") "disabled"
                        ]
                        [ text "追加" ]
                    ]
                ]

            AddWorkModal processId partId name ->
                [ div [ class "header" ] [ text "作業を追加" ]
                , div [ class "content" ]
                    [ Html.p []
                        [ text "名前："
                        , div [ class "ui input select-all" ]
                            [ input
                                [ type_ "text"
                                , placeholder "名前を入力"
                                , onInput <|
                                    ModalState
                                        << AddWorkModal processId partId
                                , value name
                                ]
                                []
                            ]
                        ]
                    ]
                , div [ class "actions" ]
                    [ div [ class "ui deny button" ] [ text "キャンセル" ]
                    , div
                        [ class "ui positive button"
                        , classIf (name == "") "disabled"
                        ]
                        [ text "追加" ]
                    ]
                ]

            Hidden ->
                []


isSelected : Set SomeId -> Work -> Bool
isSelected selection work =
    Set.member work.id selection
        || Set.member (unId work.process) selection
        || List.any (\partId -> Set.member (unId partId) selection)
            work.belongsTo


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
