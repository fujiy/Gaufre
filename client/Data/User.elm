module Data.User exposing (..)

import Dict
import Dict.Extra as Dict
import Firestore
import Firestore.Desc as Desc exposing (DocumentDesc)
import Firestore.Path.Id as Id exposing (Id, SelfId, unId)
import Html exposing (Html, a, div, img, input, node, span, text)
import Html.Attributes as Attr exposing (attribute, class, src, type_)
import Html.Events exposing (onClick)
import List.Extra as List
import Set
import Util exposing (onChangeValues)


type alias User =
    { id : SelfId
    , name : String
    , image : String
    , email : String
    }



-- Firestore


type alias Collection =
    Firestore.Collection () User


type alias Document =
    Firestore.Document () User


type alias Reference =
    Firestore.Reference () User


desc : DocumentDesc () User
desc =
    Desc.documentWithId User <|
        Desc.field "name" .name Desc.string
            >> Desc.field "image" .image Desc.string
            >> Desc.field "email" .email Desc.string



-- View


label : msg -> User -> Html msg
label msg user =
    a
        [ class "ui small image label"
        , onClick msg
        ]
        [ img [ src user.image ] []
        , text user.name
        ]


label_ : User -> Html msg
label_ user =
    div
        [ class "ui small image label" ]
        [ img [ src user.image ] []
        , text user.name
        ]


basicLabel_ : User -> Html msg
basicLabel_ user =
    div
        [ class "ui small disabled image label" ]
        [ img [ src user.image ] []
        , text user.name
        ]


avatar : User -> Html msg
avatar user =
    span []
        [ img [ class "ui avatar image", src user.image ] []
        , span [] [ text user.name ]
        ]


selectionList :
    List User
    -> List (Id User)
    -> List (Id User)
    -> Html (List (Id User))
selectionList choices actives inactives =
    let
        values =
            actives |> List.map unId
    in
    node "ui-dropdown"
        [ class "ui tiny multiple search selection dropdown"
        , attribute "multiple" ""
        , attribute "value" <| String.join "," values
        , onChangeValues |> Attr.map (List.map Id.fromString)
        ]
    <|
        [ input [ type_ "hidden", Attr.name "staffs" ] []
        , Html.i [ class "dropdown icon" ] []
        , div [ class "default text" ] [ text "メンバーを割り当てる" ]
        , div [ class "menu" ] <|
            List.map
                (\user ->
                    div
                        [ class "item"
                        , attribute "data-value" user.id
                        ]
                    <|
                        [ span [] [ text user.name ] ]
                )
                choices
        ]


list :
    Bool
    -> List User
    -> List (Id User)
    -> List (Id User)
    -> Html (List (Id User))
list editable choices actives inactives =
    if editable then
        selectionList choices actives inactives

    else
        let
            activeUsers =
                List.filterMap
                    (\id -> List.find (\user -> Id.self user == id) choices)
                    actives

            inactiveUsers =
                List.filterMap
                    (\id -> List.find (\user -> Id.self user == id) choices)
                    inactives
        in
        if List.isEmpty actives && List.isEmpty inactives then
            text "なし"

        else
            span [] <|
                List.map label_ activeUsers
                    ++ List.map basicLabel_ inactiveUsers
