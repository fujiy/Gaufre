port module Main exposing (..)

import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Element as El
import Firestore as Firestore exposing (Firestore)
import Firestore.Decode as Decode
import Firestore.Element as El
import Firestore.Update as Update
import Html exposing (..)
import Html.Events as Events
import Http
import Json.Decode as Json
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
        , page : Page
        , firestore : Firestore Data.Data
        }


type Page
    = Dashboard


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
                        , page = Dashboard
                        , firestore = firestore
                        }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        SignedIn r ->
            case msg of
                SignOut ->
                    ( model, signOut () )

                FirestoreUpdate firestore ->
                    let
                        ( fs, cmd ) =
                            Firestore.digest updatePort Data.encode firestore
                    in
                    ( SignedIn { r | firestore = fs }, cmd )

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
                [ El.layout [] <| El.map (always SignIn) Entrance.view ]

            SignedIn { auth, firestore, page } ->
                [ El.layout [] <|
                    case page of
                        Dashboard ->
                            El.map (always SignOut) <|
                                Page.Dashboard.view auth firestore
                ]
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
