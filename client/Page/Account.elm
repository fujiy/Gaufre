module Page.Account exposing (..)

import Data exposing (Auth, Data, Project, User, Work, myId, userDesc)
import Data.User as User
import File exposing (File)
import File.Select
import Firestore
import Firestore.Access as Access exposing (Accessor)
import Firestore.Lens as Lens exposing (o)
import Firestore.Update as Update exposing (Updater)
import GDrive
import Html exposing (Html, button, div, h1, img, input, table, tbody, td, text, textarea, thead, tr)
import Html.Attributes as Html exposing (class, href, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Result.Extra as Result
import Util exposing (..)


type alias Model =
    { profile : Maybe User }


init : Model
init =
    { profile = Nothing }


type Msg
    = EditProfile User
    | SaveProfile
    | SelectFile User
    | UploadImage File
    | SetImage GDrive.FileMeta
    | SaveImage String
    | SignOut
    | None


update : Auth -> Msg -> Model -> ( Model, Updater Data Msg )
update auth msg model =
    case msg of
        EditProfile new ->
            ( { model | profile = Just new }, Update.none )

        SaveProfile ->
            case model.profile of
                Just new ->
                    if isValid new then
                        ( { model | profile = Nothing }
                        , Update.set (User.me auth) userDesc new
                        )

                    else
                        ( model, Update.none )

                Nothing ->
                    ( model, Update.none )

        SelectFile me ->
            ( { model | profile = Just me }
            , Update.command <|
                \_ -> File.Select.file [ "image/*" ] UploadImage
            )

        UploadImage file ->
            ( model
            , Update.command <|
                \_ ->
                    GDrive.files_create auth.token (File.name file) [] file
                        |> Cmd.map (Result.unwrap SignOut SetImage)
            )

        SetImage file ->
            ( { model
                | profile =
                    Maybe.map
                        (\user -> { user | image = file.webContentLink })
                        model.profile
              }
            , Update.command <|
                \_ ->
                    GDrive.permissions_create auth.token
                        file.id
                        { role = GDrive.Reader, type_ = GDrive.Anyone }
                        |> Cmd.map
                            (\_ -> SaveImage file.webContentLink)
            )

        SaveImage image ->
            ( model
            , Update.map (\_ -> None) <|
                Update.modify (User.me auth) userDesc <|
                    \user ->
                        { user | image = image }
            )

        SignOut ->
            ( model, Update.none )

        None ->
            ( model, Update.none )


view : Auth -> Model -> Data -> Accessor Data (Html Msg)
view auth model data =
    flip Access.map
        (Access.access (o (User.me auth) Lens.get) data)
    <|
        \me ->
            let
                profile =
                    Maybe.withDefault me model.profile
            in
            div [ class "ui padded basic segment" ]
                [ h1 []
                    [ text "アカウント設定"
                    , button
                        [ class "ui right floated button"
                        , onClick SignOut
                        ]
                        [ text "ログアウト" ]
                    ]
                , table [ class "ui very basic table" ]
                    [ thead [] []
                    , tbody []
                        [ tr []
                            [ td [] [ text "プロフィール画像" ]
                            , td []
                                [ img
                                    [ class "ui tiny circular image"
                                    , src profile.image
                                    ]
                                    []
                                ]

                            -- [ Html.a
                            --     [ class "ui tiny circular image"
                            --     , href ""
                            --     , onClick <| SelectFile profile
                            --     ]
                            --     [ img
                            --         [ class "ui tiny image"
                            --         , src profile.image
                            --         ]
                            --         []
                            --     ]
                            -- ]
                            ]
                        , tr []
                            [ td [] [ text "名前" ]
                            , td []
                                [ div [ class "ui input" ]
                                    [ input
                                        [ type_ "text"
                                        , placeholder "名前を入力"
                                        , value profile.name
                                        , onInput <|
                                            \name ->
                                                EditProfile
                                                    { profile | name = name }
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        , tr []
                            [ td [] [ text "メールアドレス" ]
                            , td [] [ text me.email ]
                            ]
                        , tr []
                            [ td [] [ text "プロフィール" ]
                            , td []
                                [ div [ class "ui form" ]
                                    [ div [ class "field" ]
                                        [ textarea
                                            [ placeholder "プロフィールを入力"
                                            , value profile.profile
                                            , onInput <|
                                                \txt ->
                                                    EditProfile
                                                        { profile
                                                            | profile = txt
                                                        }
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , Html.p []
                    [ button
                        [ class "ui primary button"
                        , classIf
                            (model.profile
                                == Nothing
                                || model.profile
                                == Just me
                            )
                            "disabled"
                        , classIf (not <| isValid profile) "disabled"
                        , onClick SaveProfile
                        ]
                        [ text "変更を保存" ]
                    ]
                ]


isValid : User -> Bool
isValid profile =
    String.length profile.name > 0
