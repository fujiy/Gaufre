port module Main exposing (..)

import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Firestore as Firestore exposing (Firestore)
import Firestore.Decode as Decode
import Firestore.Update as Update
import GDrive
import Html exposing (..)
import Html.Events as Events
import Http
import Json.Decode as Json
import Page
import Page.Dashboard
import Page.Entrance as Entrance
import Url exposing (Url)


port setLocalStorage : ( String, String ) -> Cmd msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port authorized : ({ name : String, uid : String, token : String } -> msg) -> Sub msg


port watcherPort : Firestore.WatcherPort msg


port updatePort : Firestore.UpdaterPort msg



-- Model -----------------------------------------------------------------------


type Model
    = NotSignedIn
    | SignedIn
        { auth : Data.Auth
        , page : Page.Page
        , firestore : Firestore Data.Data
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags origin navKey =
    ( NotSignedIn, Cmd.none )



-- Update ----------------------------------------------------------------------


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | SignIn
    | SignOut
    | Authorized Auth
    | FirestoreUpdate (Firestore Data)
    | Page Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotSignedIn ->
            case msg of
                SignIn ->
                    ( model, signIn () )

                Authorized auth ->
                    let
                        ( firestore, cmd ) =
                            Firestore.update updatePort
                                Data.encode
                                (Data.initClient auth)
                                (Firestore.init Data.decode)
                    in
                    ( SignedIn
                        { auth = auth
                        , page = Page.init
                        , firestore = firestore
                        }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        SignedIn r ->
            case msg of
                SignOut ->
                    ( NotSignedIn, signOut () )

                FirestoreUpdate firestore ->
                    let
                        ( fs, cmd ) =
                            Firestore.digest updatePort Data.encode firestore
                    in
                    ( SignedIn { r | firestore = fs }, cmd )

                Page m ->
                    case m of
                        Page.SignOut ->
                            ( NotSignedIn, signOut () )

                        _ ->
                            let
                                ( page, cmd ) =
                                    Page.update r.auth m r.page
                            in
                            ( SignedIn { r | page = page }
                            , Cmd.map Page cmd
                            )

                _ ->
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



-- View ------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = "Gaufre"
    , body =
        case model of
            NotSignedIn ->
                [ Html.map (always SignIn) Entrance.view ]

            SignedIn { auth, firestore, page } ->
                [ Html.map Page <| Page.view auth firestore page ]
    }



-- Subscriptions ---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotSignedIn ->
            authorized Authorized

        SignedIn { firestore } ->
            Firestore.watch watcherPort Data.decode firestore
                |> Sub.map FirestoreUpdate



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
