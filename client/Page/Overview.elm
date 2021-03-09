module Page.Overview exposing (..)

import Browser.Navigation as Nav
import Data exposing (..)
import Data.Project as Project
import Data.User as User
import Data.Work as Work
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
import GDrive
import Html exposing (Html, button, div, input, label, node, span, table, td, text, th, thead, tr)
import Html.Attributes as Attr exposing (checked, class, colspan, placeholder, rowspan, style, type_, value)
import Html.Events exposing (onCheck, onClick, onDoubleClick, onInput)
import Html.Keyed as Keyed
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
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
    | ProcessSettingModal (Id Process) Bool
    | PartSettingModal (Id Part)
    | WorkSettingModal (List (Id Work))
    | DeleteWorkModal (List (Id Work))
    | DeletePartModal (Id Part)


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
    | WorkUpdate (List (Id Work)) Work.Update
    | ProjectUpdate Project.Update


update :
    Auth
    -> Msg
    -> { project : Int }
    -> Id Project
    -> Model
    -> ( Model, Updater Data Msg )
update auth msg m projectId model =
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

        WorkUpdate works upd ->
            ( model
            , Project.update auth projectId (Project.WorkUpdate works upd)
                |> Update.map ProjectUpdate
            )

        ProjectUpdate upd ->
            ( model
            , Project.update auth projectId upd |> Update.map ProjectUpdate
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
        (Access.access (o (Project.members project) Lens.gets) data)
    <|
        \works_ members ->
            let
                role =
                    Project.myRole project auth

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
                , modalView model project works <| IdMap.fromListSelf works_
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
                            (emptyCell (Project.myRole project auth)
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

        status =
            Work.getStatus work
    in
    case partRowSpan partIds partId work of
        0 ->
            text ""

        n ->
            td
                [ class "center aligned"
                , classIf selected "active"
                , classIf (status == Work.Complete) "positive"
                , style "cursor" "pointer"
                , rowspan n
                , onMouseDownStop <|
                    SelectWork (Id.self work) (not selectedOnly) True
                , onDragEnter <| SelectWork (Id.self work) (not selected) False
                , onDoubleClick <| MoveToWork process part work
                ]
                [ icon <| Work.iconClass status ]


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
        allWorks =
            IdMap.fromListSelf works

        selection =
            List.filter (Work.isSelected model.selection) works

        selectedIds =
            List.map Id.self selection

        staffs =
            gatherList (.staffs >> List.map (Firestore.getId >> unId))
                selection

        reviewers =
            gatherList (.reviewers >> List.map (Firestore.getId >> unId))
                selection

        statuses =
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
        [ div [ class "content" ]
            [ when authority.editStructure <|
                case
                    ( selection
                    , IdSet.toList model.selection.processes
                    , IdSet.toList model.selection.parts
                    )
                of
                    ( _, [ processId ], _ ) ->
                        button
                            [ class "ui right floated circular icon button"
                            , onClick <|
                                ModalState <|
                                    ProcessSettingModal processId False
                            ]
                            [ icon "cog" ]

                    ( _, _, [ partId ] ) ->
                        button
                            [ class "ui right floated circular icon button"
                            , onClick <|
                                ModalState <|
                                    PartSettingModal partId
                            ]
                            [ icon "cog" ]

                    ( _ :: _, _, _ ) ->
                        button
                            [ class "ui right floated circular icon button"
                            , onClick <|
                                ModalState <|
                                    (WorkSettingModal <|
                                        List.map Id.self selection
                                    )
                            ]
                            [ icon "cog" ]

                    _ ->
                        text ""
            , div [ class "header" ]
                [ text <|
                    Work.selectionTitle
                        project.processes
                        project.parts
                        allWorks
                        model.selection
                ]
            , div [ class "description" ] <|
                List.map Work.statusLabel statuses
            , if List.member Work.Waiting statuses then
                Html.map (\_ -> None) <|
                    div [ class "description" ] <|
                        span [] [ text "未完了の前工程：" ]
                            :: List.map
                                (Work.waitingStatuses
                                    project.processes
                                    project.parts
                                    allWorks
                                )
                                selection

              else
                text ""
            ]
        , when (not <| List.isEmpty selection) <|
            div [ class "content" ]
                [ div [ class "header" ] [ text "メンバー" ]
                , Html.p []
                    [ text "担当："
                    , User.list authority.manageWorkStaffs
                        members
                        (Dict.keys staffs.all |> List.map Id)
                        (Dict.keys staffs.partially |> List.map Id)
                        |> Html.map
                            (WorkUpdate selectedIds << Work.SetStaffs)
                    ]
                , Html.p []
                    [ text "チェック："
                    , User.list authority.manageWorkStaffs
                        members
                        (Dict.keys reviewers.all |> List.map Id)
                        (Dict.keys reviewers.partially |> List.map Id)
                        |> Html.map
                            (WorkUpdate selectedIds << Work.SetReviewers)
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
    -> IdMap.Map Work Work
    -> Html Msg
modalView model project workMap allWorks =
    node "ui-modal"
        [ class "ui small modal"
        , boolAttr "show" <| model.modal /= Hidden
        , boolAttr "noautofocus" True
        , onHide <| ModalState Hidden
        , onApprove <|
            case model.modal of
                AddPartModal processes newId newPart ->
                    ProjectUpdate <|
                        Project.AddPart
                            (IdMap.toList processes
                                |> List.filterMap
                                    (\( id, ( _, add ) ) ->
                                        if add then
                                            IdMap.get id project.processes
                                                |> Maybe.map (Tuple.pair id)

                                        else
                                            Nothing
                                    )
                            )
                            newId
                            newPart

                AddWorkModal processId partId name ->
                    case IdMap.get processId project.processes of
                        Just process ->
                            WorkUpdate [ Id.null ] <|
                                Work.Add processId process partId name

                        Nothing ->
                            None

                DeleteWorkModal ids ->
                    WorkUpdate ids Work.Delete

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
                                            , Attr.id <| unId pid
                                            , checked add
                                            , onCheck <| checkProcess pid
                                            ]
                                            []
                                        , label [ Attr.for <| unId pid ]
                                            [ text process.name ]
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

            ProcessSettingModal processId updateExistingWorks ->
                case IdMap.get processId project.processes of
                    Just process ->
                        [ div [ class "header" ]
                            [ text <| process.name ++ " の設定" ]
                        , div [ class "content" ] <|
                            [ text "前の工程："
                            , Work.processList project.processes
                                (List.map Id.selfId process.upstreams)
                                |> Html.map
                                    (ProjectUpdate
                                        << Project.SetProcessUpstreams
                                            updateExistingWorks
                                            processId
                                    )
                            , div
                                [ class "ui checkbox"
                                , style "margin-left" "2em"
                                ]
                                [ input
                                    [ type_ "checkbox"
                                    , Attr.id "update-existing"
                                    , checked updateExistingWorks
                                    , onCheck <|
                                        ModalState
                                            << ProcessSettingModal processId
                                    ]
                                    []
                                , label [ Attr.for "update-existing" ]
                                    [ text "既存の作業に適用する" ]
                                ]
                            ]
                        , div [ class "actions" ]
                            [ button [ class "ui positive button" ]
                                [ text "完了" ]
                            ]
                        ]

                    Nothing ->
                        []

            PartSettingModal partId ->
                case IdMap.get partId project.parts of
                    Just part ->
                        [ div [ class "header" ]
                            [ text <| part.name ++ " の設定" ]
                        , div [ class "content" ] <|
                            [ button
                                [ class "ui negative button"
                                , onClick <|
                                    ModalState <|
                                        DeletePartModal partId
                                ]
                                [ text "カットを削除" ]
                            ]
                        , div [ class "actions" ]
                            [ button [ class "ui positive button" ]
                                [ text "完了" ]
                            ]
                        ]

                    Nothing ->
                        []

            WorkSettingModal workIds ->
                let
                    works =
                        List.filterMap (flip IdMap.get allWorks) workIds

                    title =
                        Work.worksTitle project.processes
                            project.parts
                            allWorks
                            workIds
                in
                [ div [ class "header" ]
                    [ text <| title ++ " の設定" ]
                , case works of
                    [ work ] ->
                        div [ class "content" ] <|
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
                                |> Html.map
                                    (WorkUpdate [ Id.self work ]
                                        << Work.SetBelongsTo
                                    )
                            ]

                    _ ->
                        text ""
                , div [ class "content" ] <|
                    [ button
                        [ class "ui negative button"
                        , onClick <| ModalState <| DeleteWorkModal workIds
                        ]
                        [ text "作業を削除" ]
                    ]
                , div [ class "actions" ]
                    [ button [ class "ui positive button" ]
                        [ text "完了" ]
                    ]
                ]

            DeleteWorkModal workIds ->
                let
                    title =
                        Work.worksTitle project.processes
                            project.parts
                            allWorks
                            workIds
                in
                [ div [ class "ui icon header" ]
                    [ icon "trash alternate outline"
                    , text <| title ++ " を削除"
                    ]
                , div [ class "content" ]
                    [ Html.p [] [ text "本当に削除しますか？" ] ]
                , div [ class "actions" ]
                    [ button
                        [ class "ui negative button"
                        , onClick <|
                            WorkUpdate workIds Work.Delete
                        ]
                        [ text "削除" ]
                    , button [ class "ui deny button" ]
                        [ text "キャンセル" ]
                    ]
                ]

            DeletePartModal partId ->
                case IdMap.get partId project.parts of
                    Just part ->
                        [ div [ class "ui icon header" ]
                            [ icon "trash alternate outline"
                            , text <| part.name ++ " を削除"
                            ]
                        , div [ class "content" ]
                            [ Html.p [] [ text "本当に削除しますか？" ] ]
                        , div [ class "actions" ]
                            [ button
                                [ class "ui negative button"
                                , onClick <|
                                    ProjectUpdate <|
                                        Project.DeletePart partId
                                ]
                                [ text "削除" ]
                            , button [ class "ui deny button" ]
                                [ text "キャンセル" ]
                            ]
                        ]

                    Nothing ->
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
