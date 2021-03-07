module Page.Members exposing (..)

import Data exposing (Auth, Data)
import Data.Project as Project exposing (Project)
import Data.User exposing (User)
import Firestore exposing (Id)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Path.Id as Id exposing (Id)
import Firestore.Update as Update exposing (Updater)
import Html exposing (Html, button, div, img, input, node, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, placeholder, src, type_)
import Html.Events as Events exposing (onClick, onInput)
import Json.Decode as Decode
import Util exposing (..)


type alias Model =
    { modal : Modal
    }


type Modal
    = Hidden
    | SearchUserModal String


init : Model
init =
    { modal = Hidden }


type Msg
    = ModalState Modal
    | InviteUser String
    | InvitedUser (Maybe User)


update :
    Auth
    -> Msg
    -> Id Project
    -> Model
    -> ( Model, Updater Data Msg )
update auth msg projectId model =
    case msg of
        ModalState modal ->
            ( { model | modal = modal }, Update.none )

        InviteUser email ->
            ( { model | modal = Hidden }
            , Data.inviteMember auth projectId email
                |> Update.map InvitedUser
            )

        InvitedUser _ ->
            ( model, Update.none )


view : Auth -> Model -> Data -> Project -> Accessor Data (Html Msg)
view auth model data project =
    flip Access.map
        (Access.access (o (Data.projectMembers project) Lens.gets) data)
    <|
        \members ->
            div []
                [ table [ class "ui sinigle line table" ]
                    [ thead [] [ tableHeader <| Data.myRole project auth ]
                    , tbody [] <| List.map (tableRow project) members
                    ]
                , modalView model project
                ]


tableHeader : Project.Role -> Html Msg
tableHeader role =
    tr []
        [ th [ colspan 3 ]
            [ text "メンバー"
            , when (Project.authority role).manageMembers <|
                button
                    [ class "ui right floated primary button"
                    , onClick <| ModalState <| SearchUserModal ""
                    ]
                    [ icon "user plus", text "メンバーを招待する" ]
            ]
        ]


tableRow : Project -> User -> Html msg
tableRow project user =
    tr []
        [ td [ class "collapsing" ]
            [ img [ class "ui avatar image", src user.image ] [] ]
        , td []
            [ div [ class "header" ] [ text user.name ] ]
        , td [ class "collapsing" ]
            [ case Data.userRole project <| Id.self user of
                Project.Owner ->
                    text "プロジェクトオーナー"

                Project.Admin ->
                    text "管理者"

                Project.Staff ->
                    text ""
            ]
        ]


modalView : Model -> Project -> Html Msg
modalView model project =
    node "ui-modal"
        [ boolAttr "show" <| model.modal /= Hidden
        , class "ui small modal"
        , Events.on "hide" <| Decode.succeed <| ModalState Hidden

        -- , Events.on "approve" <|
        --     Decode.succeed <|
        --         case model.modal of
        --             _ ->
        --                 None
        ]
    <|
        case model.modal of
            SearchUserModal email ->
                [ div [ class "header" ] [ text "メンバーを招待する" ]
                , div [ class "content" ]
                    [ div [ class "ui right labeled input select-all" ]
                        [ input
                            [ type_ "text"
                            , placeholder "メールアドレス"
                            , onInput <| ModalState << SearchUserModal
                            ]
                            []

                        -- , icon "search"
                        , div [ class "ui label" ] [ text "@gmail.com" ]
                        ]
                    , button
                        [ class "ui primary labeled right floated icon button"
                        , classIf (email == "") "disabled"
                        , onClick <| InviteUser email
                        ]
                        [ icon "user plus", text "招待" ]
                    ]
                ]

            _ ->
                []
