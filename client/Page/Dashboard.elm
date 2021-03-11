module Page.Dashboard exposing (..)

import Data exposing (Auth, Data, Project, User, Work, myId)
import Data.Client as Client exposing (TaskState(..))
import Data.Project as Project
import Data.Work as Work
import Firestore
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path.Id as Id exposing (Id)
import Firestore.Path.Id.Map as IdMap
import Firestore.Remote as Remote
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, div, text)
import Html.Attributes as Html exposing (class, href, style)
import Util exposing (..)


type Model
    = Model


init : Model
init =
    Model


type Msg
    = None


update : Auth -> Msg -> Model -> ( Model, Updater Data Msg )
update auth msg model =
    ( model, Update.none )


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    Access.map
        (div
            [ class "ui six cards"
            , style "overflow-x" "scroll"
            , style "flex-wrap" "nowrap"
            , style "height" "100vh"
            , style "margin" "0"
            , style "margin-right" "10px"
            ]
        )
    <|
        Access.andThen
            (\members_ ->
                let
                    members =
                        IdMap.fromListSelf members_
                in
                Access.list <|
                    flip List.map Client.defaultList <|
                        taskList auth data project members
            )
            (Access.access (o (Project.members project) Lens.gets) data)


taskList :
    Auth
    -> Data
    -> Project
    -> IdMap.Map User User
    -> TaskState
    -> Accessor Data (Html Msg)
taskList auth data project members task =
    flip Access.map
        (taskWorks auth data (Id.self project) task)
        (\works_ ->
            let
                works =
                    List.sortWith (Work.compare project) works_
            in
            div
                [ class "ui card"
                , class <| Client.taskColor task
                , style "flex" "0 0 300px"
                ]
                [ div [ class "content" ]
                    [ div [ class "header" ]
                        [ icon <|
                            Client.taskIconClass task
                                ++ " "
                                ++ Client.taskColor task
                        , text <| Client.taskTitle task
                        ]
                    , div [ class "meta" ]
                        [ text <| String.fromInt <| List.length works
                        , text "件"
                        ]
                    , div
                        [ class "ui link divided items"
                        , style "overflow-y" "scroll"
                        , style "overflow-x" "hidden"
                        , style "max-height" "calc(100vh - 120px)"
                        ]
                      <|
                        flip List.map works <|
                            \work ->
                                let
                                    link =
                                        Work.relativeLink project work
                                in
                                Html.a
                                    [ class "item", href link ]
                                    [ div [ class "content" ]
                                        [ div [ class "header" ]
                                            [ text <| Work.title project work ]
                                        , div [ class "description" ]
                                            [ Html.p [] [] ]
                                        , div [ class "extra" ]
                                            [ extraInfo members work ]
                                        ]
                                    ]
                    ]
                ]
        )


extraInfo : IdMap.Map User User -> Work -> Html msg
extraInfo members work =
    case Work.getStatus work of
        _ ->
            text ""


taskWorks :
    Auth
    -> Data
    -> Id Project
    -> TaskState
    -> Accessor Data (List Work)
taskWorks auth data projectId task =
    let
        lens l =
            o (Data.project projectId) <|
                o Project.works <|
                    o (l <| myId auth) Lens.getAll
    in
    Access.map (Remote.withDefault []) <|
        Access.remote <|
            case task of
                Working ->
                    Access.access (lens Work.isWorking) data

                Reviewing ->
                    Access.access (lens Work.isReviewing) data

                WaitingUpstream ->
                    Access.access (lens Work.isWaitingUpstream) data

                WaitingSubmit ->
                    Access.access (lens Work.isWaitingSubmit) data

                WaitingReview ->
                    Access.access (lens Work.isWaitingReview) data

                Complete ->
                    Access.map2 (++)
                        (Access.access (lens Work.isCompletedAsStaff) data)
                        (Access.access (lens Work.isCompletedAsReviewer) data)
