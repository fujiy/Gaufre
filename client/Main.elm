port module Main exposing (..)

import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events as Events
import Http
import Url exposing (Url)


port setLocalStorage : ( String, String ) -> Cmd msg
port signIn : () -> Cmd msg
port signOut : () -> Cmd msg
port authorized : ({ user : User, token: String } -> msg) -> Sub msg


-- Model -----------------------------------------------------------------------

type alias User = { name : String }

type Model
    = NotSignedIn
    | SignedIn
        { token : String
        , user: User
        }

type Page
    = Home


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags origin navKey = ( NotSignedIn, Cmd.none)

-- Update ----------------------------------------------------------------------


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | SignIn
    | SignOut
    | Authorized { user : User, token: String }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotSignedIn ->
            case msg of
                SignIn ->
                    ( model, signIn ())
                Authorized r ->
                    (SignedIn r, Cmd.none)
                _ -> ( model, Cmd.none )
        SignedIn _ ->
            case msg of
                SignOut ->
                    ( model, signOut ())
                _ -> ( model, Cmd.none )


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


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b



-- View ------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = "Gaufre - Sign In"
    , body =
        case model of
            NotSignedIn ->
                [ text "idle"
                , button [ Events.onClick SignIn]
                    [ text "sign in" ]
                ]

            SignedIn {user, token } ->
                [ text user.name
                , text token
                , button [ Events.onClick SignOut]
                    [text "sign out"]
                ]
    }



-- Subscriptions ---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    authorized Authorized



-- Main ------------------------------------------------------------------------


main : Program () Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
