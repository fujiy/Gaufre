module Page.Projects exposing (..)

import Data exposing (Auth, Data)
import GDrive
import Html exposing (Html, a, div, i, input, node, span, text)
import Html.Attributes as Html exposing (attribute, class, placeholder, style, type_)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Util exposing (..)


type Model
    = List
    | SearchFolder
        { loading : Bool
        , wait : Maybe String
        , result : List GDrive.FileMeta
        }


type Msg
    = ShowModal
    | HideModal
    | Search String
    | SearchResult (List GDrive.FileMeta)


update : Auth -> Msg -> Model -> ( Model, Cmd Msg )
update auth msg model =
    case ( msg, model ) of
        ( ShowModal, _ ) ->
            ( SearchFolder { loading = False, wait = Nothing, result = [] }
            , Cmd.none
            )

        ( HideModal, _ ) ->
            ( List, Cmd.none )

        ( Search "", _ ) ->
            ( model, Cmd.none )

        ( Search s, SearchFolder o ) ->
            if o.loading then
                ( SearchFolder { o | wait = Just s }, Cmd.none )

            else
                ( SearchFolder { o | loading = True, wait = Nothing }
                , GDrive.folders auth.token s
                    |> Cmd.map
                        (\r ->
                            case r of
                                Err error ->
                                    always (SearchResult []) <|
                                        Debug.log "error" error

                                Ok files ->
                                    SearchResult files
                        )
                )

        ( SearchResult files, SearchFolder o ) ->
            case o.wait of
                Nothing ->
                    ( SearchFolder { o | loading = False, result = files }
                    , Cmd.none
                    )

                Just s ->
                    ( SearchFolder
                        { o | loading = True, wait = Nothing, result = files }
                    , GDrive.folders auth.token s
                        |> Cmd.map
                            (\r ->
                                case r of
                                    Err error ->
                                        always (SearchResult []) <|
                                            Debug.log "error" error

                                    Ok files_ ->
                                        SearchResult files_
                            )
                    )

        _ ->
            ( model, Cmd.none )


view : Auth -> Data -> Model -> Html Msg
view auth data model =
    let
        showModal =
            case model of
                SearchFolder _ ->
                    True

                _ ->
                    False
    in
    div []
        [ div [ class "ui cards", style "margin" "20px" ] <|
            a
                [ class "ui card centered"
                , onClick ShowModal
                ]
                [ div [ class "content" ]
                    [ div [ class "center aligned header" ]
                        [ i [ class "plus icon", style "margin" "20px" ] [] ]
                    , div [ class "center aligned header" ]
                        [ text "Add Project" ]
                    ]
                ]
                :: []
        , node "ui-modal"
            [ boolAttr "show" showModal
            , class "ui tiny modal"
            , on "hide" <| Decode.succeed HideModal
            ]
            [ div [ class "header" ]
                [ text "Select a project folder in your Google Drive" ]
            , case model of
                SearchFolder { result, loading } ->
                    div [ class "content" ]
                        [ div
                            [ class "ui fluid search"
                            , classIf loading "loading"
                            ]
                            [ div [ class "ui icon fluid input" ]
                                [ input
                                    [ class "propt"
                                    , type_ "text"
                                    , placeholder "Folder names..."
                                    , onInput Search
                                    ]
                                    []
                                , i [ class "search icon" ] []
                                ]
                            ]
                        , div [ class "ui link items" ] <|
                            flip List.map result <|
                                \file ->
                                    div [ class "item" ]
                                        [ a [ class "header" ]
                                            [ text file.name ]
                                        ]
                        ]

                _ ->
                    text ""
            ]
        ]
