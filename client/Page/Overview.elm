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
import Firestore.Path.Id as Id exposing (Id(..), unId)
import Firestore.Path.Id.Map as IdMap
import Firestore.Path.Id.Set as IdSet
import Firestore.Remote exposing (Remote(..))
import Firestore.Update as Update exposing (Updater)
import GDrive exposing (FileMeta)
import Html exposing (Html, button, div, input, label, node, table, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, colspan, placeholder, rowspan, style, type_, value)
import Html.Events exposing (onCheck, onClick, onDoubleClick, onInput)
import Html.Keyed as Keyed
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Set exposing (Set)
import Url.Builder as Url
import Util exposing (..)
import View.Button as Button


type alias Model =
    { selection : Work.Selection
    , modal : Modal
    }


type Modal
    = Hidden
    | AddPartModal (IdMap.Map Process ( Process, Bool )) (Id Part) Part
    | AddWorkModal (Id Process) (Id Part) String
    | ProcessSettingModal (Id Process)
      -- | PartSettingModal (Id Part)
    | WorkSettingModal (Id Work)
    | DeleteWorkModal (Id Work)


init : Model
init =
    { selection = noSelection, modal = Hidden }


noSelection : Work.Selection
noSelection =
    Work.Selection IdSet.empty IdSet.empty IdSet.empty


type Msg
    = None
    | ModalState Modal
    | MoveToWork Process Part Work
    | SelectWork (Id Work) Bool Bool
    | SelectProcess (Id Process) Bool Bool
    | SelectPart (Id Part) Bool Bool
    | ClearSelection
    | AddPart (List (Id Process)) (Id Part) Part
    | AddWork (Id Process) (Id Part) String
    | CreatedWorkFolder (Id Process) (Id Part) FileMeta
    | SetWorkStaffs (List Work) (List (Id User))
    | SetWorkReviewers (List Work) (List (Id User))
    | SetWorkBelongsTo Work (List (Id Part))
    | DeleteWork (Id Work)
    | SetProcessUpstreams (Id Process) (List (Id Process))


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

        SelectWork id select clear ->
            ( { model
                | selection =
                    { noSelection
                        | works =
                            IdSet.change id select clear model.selection.works
                    }
              }
            , Update.none
            )

        SelectProcess id select clear ->
            ( { model
                | selection =
                    { noSelection
                        | processes =
                            IdSet.change id select clear model.selection.processes
                    }
              }
            , Update.none
            )

        SelectPart id select clear ->
            ( { model
                | selection =
                    { noSelection
                        | parts =
                            IdSet.change id select clear model.selection.parts
                    }
              }
            , Update.none
            )

        ClearSelection ->
            ( { model | selection = noSelection }
            , Update.none
            )

        AddPart processes newId newPart ->
            ( model
            , Update.all
                [ Update.modify projectLens Project.desc <|
                    \p -> { p | parts = IdMap.insert newId newPart p.parts }
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

        DeleteWork workId ->
            ( model
            , Update.all
                [ Update.delete
                    (o projectLens <| Project.work workId)
                    Work.desc
                , Update.command <|
                    \_ ->
                        GDrive.files_update auth.token
                            (unId workId)
                            { gdriveUpdate | trashed = Just True }
                            |> Cmd.map (\_ -> None)
                ]
            )

        SetProcessUpstreams processId upstreams ->
            ( model
            , Update.modify projectLens Project.desc <|
                \project ->
                    { project
                        | processes =
                            IdMap.modify processId
                                (\process ->
                                    { process
                                        | upstreams =
                                            List.map unId upstreams
                                    }
                                )
                                project.processes
                    }
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
            IdMap.toList project.processes
                |> List.sortBy (\( _, p ) -> p.order)

        parts =
            IdMap.toList project.parts
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
                        |> IdMap.groupBy Tuple.first
                        |> IdMap.map (List.map Tuple.second >> IdMap.fromList)

                partIds =
                    List.map Tuple.first parts

                addPartButton =
                    if Project.authority role |> .editStructure then
                        [ ( "-1"
                          , tr []
                                [ td
                                    [ colspan <| List.length processes ]
                                    [ Button.add <| addPart project ]
                                ]
                          )
                        ]

                    else
                        []
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
                    , Keyed.node "tbody" [] <|
                        List.map
                            (\( partId, part ) ->
                                ( unId partId
                                , tableRow auth
                                    model
                                    project
                                    partIds
                                    processes
                                    works
                                    partId
                                    part
                                )
                            )
                            parts
                            ++ addPartButton
                    ]
                , actions model project role members works_
                , modalView model project works works_
                ]


tableHeader : Model -> List ( Id Process, Process ) -> Html Msg
tableHeader model processes =
    tr [] <|
        th [] []
            :: List.map
                (\( id, process ) ->
                    let
                        selected =
                            IdSet.member id model.selection.processes
                    in
                    th
                        [ class "selectable collapsing"
                        , classIf selected "active"
                        , style "cursor" "pointer"
                        , onMouseDownStop <|
                            SelectProcess id (not selected) True
                        , onDragEnter <|
                            SelectProcess id (not selected) False
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
    -> IdMap.Map Process (IdMap.Map Part Work)
    -> Id Part
    -> Part
    -> Html Msg
tableRow auth model project partIds processes works partId part =
    let
        selected =
            IdSet.member partId model.selection.parts
    in
    Keyed.node "tr" [] <|
        ( "0"
        , td
            [ class "selectable right aligned"
            , classIf selected "active"
            , style "padding" "5px"
            , style "cursor" "pointer"
            , onMouseDownStop <| SelectPart partId (not selected) True
            , onDragEnter <| SelectPart partId (not selected) False
            ]
            [ text part.name ]
        )
            :: List.map
                (\( processId, process ) ->
                    let
                        parts =
                            IdMap.get processId works
                                |> Maybe.withDefault IdMap.empty
                    in
                    ( unId processId ++ unId partId
                    , IdMap.get partId parts
                        |> Maybe.unwrap
                            (emptyCell (Data.myRole project auth)
                                processId
                                partId
                                part
                            )
                            (workCell model partIds process partId part)
                    )
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
            Work.isSelected model.selection work

        selectedOnly =
            model.selection.works == IdSet.singleton (Id.self work)
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
                , onMouseDownStop <|
                    SelectWork (Id.self work) (not selectedOnly) True
                , onDragEnter <| SelectWork (Id.self work) (not selected) False
                , onDoubleClick <| MoveToWork process part work
                ]
                [ icon <| Work.iconClass <| Work.getStatus work ]


partRowSpan : List (Id Part) -> Id Part -> Work -> Int
partRowSpan ps partId work =
    let
        workParts =
            work.belongsTo |> IdSet.fromList

        goBefore parts =
            case parts of
                [] ->
                    0

                id :: parts_ ->
                    if IdSet.member id workParts then
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

                    else if IdSet.member id workParts then
                        goSkip parts_

                    else
                        goBefore parts_

        goAfter parts =
            case parts of
                [] ->
                    1

                id :: parts_ ->
                    if IdSet.member id workParts then
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
    -> List Work
    -> Html Msg
actions model project role members works =
    let
        selection =
            List.filter (Work.isSelected model.selection) works

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
                [ when authority.editStructure <|
                    case
                        ( selection
                        , IdSet.toList model.selection.processes
                        , IdSet.toList model.selection.parts
                        )
                    of
                        ( [ work ], _, _ ) ->
                            button
                                [ class "ui right floated circular icon button"
                                , onClick <|
                                    ModalState
                                        (WorkSettingModal <| Id.self work)
                                ]
                                [ icon "cog" ]

                        ( _, [ processId ], _ ) ->
                            button
                                [ class "ui right floated circular icon button"
                                , onClick <|
                                    ModalState
                                        (ProcessSettingModal processId)
                                ]
                                [ icon "cog" ]

                        _ ->
                            text ""
                , div [ class "header" ]
                    [ text <|
                        Work.selectionTitle
                            project.processes
                            project.parts
                            works
                            model.selection
                    ]
                , div [ class "description" ] <|
                    List.map Work.statusLabel status
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
            ]


addPart : Project -> Msg
addPart project =
    let
        ( newId, newPart ) =
            Project.newPart project

        processes =
            flip IdMap.map project.processes <| \process -> ( process, True )
    in
    ModalState <| AddPartModal processes newId newPart


modalView :
    Model
    -> Project
    -> IdMap.Map Process (IdMap.Map Part Work)
    -> List Work
    -> Html Msg
modalView model project workMap works =
    node "ui-modal"
        [ class "ui small modal"
        , boolAttr "show" <| model.modal /= Hidden
        , boolAttr "noautofocus" True
        , onHide <| ModalState Hidden
        , onApprove <|
            case model.modal of
                AddPartModal processes newId newPart ->
                    AddPart
                        (IdMap.toList processes
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

                DeleteWorkModal work ->
                    DeleteWork work

                _ ->
                    None
        ]
    <|
        case model.modal of
            AddPartModal processes id part ->
                let
                    processList =
                        IdMap.toList processes
                            |> List.sortBy (\( _, ( p, _ ) ) -> p.order)

                    checkProcess processId check =
                        ModalState <|
                            AddPartModal
                                (IdMap.modify processId
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
                    [ button [ class "ui deny button" ] [ text "キャンセル" ]
                    , button
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
                    , button
                        [ class "ui positive button"
                        , classIf (name == "") "disabled"
                        ]
                        [ text "追加" ]
                    ]
                ]

            ProcessSettingModal processId ->
                case IdMap.get processId project.processes of
                    Just process ->
                        [ div [ class "header" ]
                            [ text <| process.name ++ " の設定" ]
                        , div [ class "content" ] <|
                            [ text "前の工程："
                            , Work.processList project.processes
                                (List.map Id.selfId process.upstreams)
                                |> Html.map (SetProcessUpstreams processId)
                            ]
                        , div [ class "actions" ]
                            [ button [ class "ui positive button" ]
                                [ text "完了" ]
                            ]
                        ]

                    Nothing ->
                        []

            WorkSettingModal workId ->
                case List.find (Id.self >> (==) workId) works of
                    Just work ->
                        let
                            title =
                                Work.title project.processes project.parts work
                        in
                        [ div [ class "header" ]
                            [ text <| title ++ " の設定" ]
                        , div [ class "content" ] <|
                            let
                                parts =
                                    IdMap.get work.process workMap
                                        |> Maybe.withDefault IdMap.empty

                                choices =
                                    IdMap.filter
                                        (\id _ ->
                                            not (IdMap.member id parts)
                                                || List.member id
                                                    work.belongsTo
                                        )
                                        project.parts
                            in
                            [ text "含まれるカット："
                            , Work.partList choices work
                                |> Html.map (SetWorkBelongsTo work)
                            ]
                        , div [ class "content" ] <|
                            [ button
                                [ class "ui negative button"
                                , onClick <|
                                    ModalState <|
                                        DeleteWorkModal workId
                                ]
                                [ text "作業を削除" ]
                            ]
                        , div [ class "actions" ]
                            [ button [ class "ui positive button" ]
                                [ text "完了" ]
                            ]
                        ]

                    Nothing ->
                        []

            DeleteWorkModal workId ->
                case List.find (Id.self >> (==) workId) works of
                    Just work ->
                        [ div [ class "ui icon header" ]
                            [ icon "trash alternate outline"
                            , text <| work.name ++ " を削除"
                            ]
                        , div [ class "content" ]
                            [ Html.p [] [ text "本当に削除しますか？" ] ]
                        , div [ class "actions" ]
                            [ button
                                [ class "ui negative button"
                                , onClick <| DeleteWork workId
                                ]
                                [ text "削除" ]
                            , button [ class "ui deny button" ]
                                [ text "キャンセル" ]
                            ]
                        ]

                    _ ->
                        []

            Hidden ->
                []


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


gdriveUpdate : GDrive.Update
gdriveUpdate =
    GDrive.update
