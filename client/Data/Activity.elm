module Data.Activity exposing (..)

import Data exposing (..)
import Data.User as User
import Firestore exposing (compareTimestamp)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, unId)
import Firestore.Path.Id.Map as IdMap
import Html exposing (Html, div, img, pre, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Tree exposing (Tree)
import Util exposing (flip, timeAgo, timeDistance, when)


type alias Collection =
    Firestore.Collection () Activity


type alias Document =
    Firestore.Document () Activity


type alias Reference =
    Firestore.Reference () Activity


type Update
    = AddComment String
    | DeleteComment
    | CancelSubmission


ref : Id Project -> Id Work -> Id Activity -> Reference
ref pid wid id =
    Firestore.ref <|
        Path.fromIds
            [ "projects", unId pid, "works", unId wid, "activities", unId id ]


tree :
    Auth
    -> IdMap.Map User User
    -> List (Tree Activity)
    -> Html ( Id Activity, Update )
tree auth users activities =
    let
        xs =
            List.sortWith
                (\x y ->
                    compareTimestamp
                        (Tree.label x |> .createdAt)
                        (Tree.label y |> .createdAt)
                )
                activities
    in
    div [ class "ui comments" ] <|
        List.map (item auth users) xs


item :
    Auth
    -> IdMap.Map User User
    -> Tree Activity
    -> Html ( Id Activity, Update )
item auth users t =
    let
        a =
            Tree.label t

        user =
            IdMap.get (Firestore.getId a.author) users
                |> Maybe.withDefault User.unknown

        isMine =
            Firestore.getId a.author == myId auth

        date =
            Firestore.toPosix a.createdAt
                |> Maybe.map (flip timeAgo auth.now)
                |> Maybe.withDefault ""
    in
    div [ class "comment" ]
        [ div [ class "avatar" ] [ img [ src user.image ] [] ]
        , div [ class "content" ]
            [ span [ class "author" ] [ text user.name ]
            , div [ class "metadata" ]
                [ div [ class "rating" ]
                    [ case a.type_ of
                        Comment ->
                            text ""

                        Submission ->
                            text "提出しました"

                        Review ->
                            span []
                                [ text "チェックしました: "
                                , if a.reject then
                                    text "リテイク"

                                  else
                                    text "OK"
                                ]
                    ]
                , div [ class "date" ] [ text date ]
                ]
            , pre [ class "text" ] [ text a.text ]
            , div [ class "actions" ]
                [ Html.a [] [ text "返信" ]
                , when isMine <|
                    case a.type_ of
                        Comment ->
                            Html.a [ onClick ( Id.self a, DeleteComment ) ]
                                [ text "削除" ]

                        Submission ->
                            Html.a [ onClick ( Id.self a, CancelSubmission ) ]
                                [ text "取り消す" ]

                        Review ->
                            Html.a [ onClick ( Id.self a, DeleteComment ) ]
                                [ text "" ]
                ]
            ]
        , tree auth users <| Tree.children t
        ]


replyTree : List Activity -> List (Tree Activity)
replyTree =
    let
        insert :
            Activity
            -> List (Tree Activity)
            -> ( Bool, List (Tree Activity) )
        insert a ts =
            case a.replyTo of
                Nothing ->
                    ( True, Tree.singleton a :: ts )

                Just (ActivityRef r) ->
                    case ts of
                        [] ->
                            ( False, [] )

                        t :: ts_ ->
                            if Id.self (Tree.label t) == Firestore.getId r then
                                ( True
                                , Tree.prependChild (Tree.singleton a) t :: ts_
                                )

                            else
                                let
                                    ( inserted, ts__ ) =
                                        insert a ts_
                                in
                                if inserted then
                                    ( True, t :: ts__ )

                                else
                                    let
                                        ( inserted_, tts ) =
                                            insert a <| Tree.children t
                                    in
                                    ( inserted
                                    , Tree.replaceChildren tts t :: ts__
                                    )
    in
    List.foldr (\a -> insert a >> Tuple.second) []
