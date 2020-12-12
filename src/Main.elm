port module Main exposing (..)

import Base64
import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Html exposing (..)
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as JE
import OAuth
import OAuth.AuthorizationCode.PKCE as OAuth
import Random
import Task
import Url exposing (Url)


port setLocalStorage : ( String, String ) -> Cmd msg



-- Model -----------------------------------------------------------------------


type Model
    = NotSignedIn
        { flow : Flow
        , redirectUri : Url
        , credentials : OAuth.Credentials
        }
    | SignedIn { token : OAuth.Token }


type Flow
    = Idle
    | Failed String
    | Authorized OAuth.AuthorizationCode OAuth.CodeVerifier
    | Authenticated OAuth.Token


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags origin navKey =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Nav.replaceUrl navKey (Url.toString redirectUri)

        mCodeVerifier =
            (Base64.toBytes <| flags.codeVerifier)
                |> Maybe.andThen OAuth.codeVerifierFromBytes

        notSignedIn flow =
            NotSignedIn
                { flow = flow
                , redirectUri = redirectUri
                , credentials = flags.credentials
                }
    in
    case OAuth.parseCode origin of
        OAuth.Empty ->
            ( notSignedIn Idle
            , Cmd.none
            )

        OAuth.Success { code, state } ->
            if state /= Just flags.state then
                ( notSignedIn <|
                    Failed <|
                        "mismatch state"
                            ++ Maybe.withDefault "NOSTATE" state
                            ++ " vs "
                            ++ flags.state
                , clearUrl
                )

            else
                case mCodeVerifier of
                    Nothing ->
                        ( notSignedIn <| Failed "no code verifier", clearUrl )

                    Just codeVerifier ->
                        ( notSignedIn (Authorized code codeVerifier)
                        , Cmd.batch
                            [ Task.succeed AccessTokenRequested
                                |> Task.perform identity
                            , clearUrl
                            ]
                        )

        OAuth.Error error ->
            ( notSignedIn <| Failed "oauth error", clearUrl )


type alias Flags =
    { credentials : OAuth.Credentials
    , codeVerifier : String
    , state : String
    }



-- Update ----------------------------------------------------------------------


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
      -- Auth
    | SignInRequested
    | GotAuthRandom { state : String, codeVerifier : Bytes }
    | AccessTokenRequested
    | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)


updateAuth :
    Msg
    ->
        { flow : Flow
        , redirectUri : Url
        , credentials : OAuth.Credentials
        }
    -> ( Flow, Cmd Msg )
updateAuth msg { flow, credentials, redirectUri } =
    case ( flow, msg ) of
        ( Idle, SignInRequested ) ->
            ( flow, genAuthRandom )

        ( Idle, GotAuthRandom { state, codeVerifier } ) ->
            case OAuth.codeVerifierFromBytes codeVerifier of
                Nothing ->
                    ( Failed "no codeVerifier", Cmd.none )

                Just cv ->
                    let
                        authorization =
                            { clientId = credentials.clientId
                            , redirectUri = redirectUri
                            , scope = authScope
                            , state = Just state
                            , codeChallenge = OAuth.mkCodeChallenge cv
                            , url = endpoint.authorization
                            }
                    in
                    ( flow
                    , Cmd.batch
                        [ setLocalStorage ( "state", state )
                        , setLocalStorage
                            ( "codeVerifier"
                            , Base64.fromBytes codeVerifier
                                |> Maybe.withDefault ""
                            )
                        , authorization
                            |> OAuth.makeAuthorizationUrl
                            |> Url.toString
                            |> Nav.load
                        ]
                    )

        ( Authorized code codeVerifier, AccessTokenRequested ) ->
            ( flow
            , Http.request <|
                OAuth.makeTokenRequest GotAccessToken
                    { credentials = credentials
                    , code = code
                    , codeVerifier = codeVerifier
                    , url = endpoint.token
                    , redirectUri = redirectUri
                    }
            )

        ( Authorized _ _, GotAccessToken authenticationResponse ) ->
            case authenticationResponse of
                Err _ ->
                    ( Failed "responce error", Cmd.none )

                Ok { token } ->
                    ( Authenticated token, Cmd.none )

        _ ->
            ( flow, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotSignedIn o ->
            let
                ( flow, cmd ) =
                    updateAuth msg o
            in
            case flow of
                Authenticated token ->
                    ( SignedIn { token = token }, cmd )

                _ ->
                    ( NotSignedIn { o | flow = flow }, cmd )

        SignedIn _ ->
            ( model, Cmd.none )


https : Url
https =
    { protocol = Url.Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


endpoint =
    { authorization =
        { https | host = "accounts.google.com", path = "/o/oauth2/v2/auth" }
    , token =
        { https | host = "oauth2.googleapis.com", path = "/token" }
    }


authScope =
    [ "https://www.googleapis.com/auth/drive.appdata"
    ]


cSTATE_SIZE : Int
cSTATE_SIZE =
    8


cCODE_VERIFIER_SIZE : Int
cCODE_VERIFIER_SIZE =
    32


genAuthRandom : Cmd Msg
genAuthRandom =
    Random.map2 (\x y -> GotAuthRandom { state = x, codeVerifier = y })
        (randomBytes cSTATE_SIZE
            |> Random.map (Base64.fromBytes >> Maybe.withDefault "")
        )
        (randomBytes cCODE_VERIFIER_SIZE)
        |> Random.generate identity


randomBytes : Int -> Random.Generator Bytes
randomBytes n =
    Random.list n (Random.int 0 255)
        |> Random.map (List.map BE.unsignedInt8 >> BE.sequence >> BE.encode)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b



-- View ------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = "Gaufre"
    , body =
        case model of
            NotSignedIn { flow } ->
                case flow of
                    Idle ->
                        [ text "idle"
                        , button [ Events.onClick SignInRequested ]
                            [ text "sign in" ]
                        ]

                    Failed err ->
                        [ text "failed", text err ]

                    Authorized _ _ ->
                        [ text "auth" ]

                    _ ->
                        []

            SignedIn { token } ->
                [ text <| OAuth.tokenToString token ]
    }



-- Subscriptions ---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Main ------------------------------------------------------------------------


main : Program Flags Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
