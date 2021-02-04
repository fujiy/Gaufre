port module Main exposing (..)

import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Firestore as Firestore exposing (Firestore)
import Firestore.Access as Access
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
        , view : Document Msg
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
                        ( firestore, mview, cmd ) =
                            Firestore.update
                                updatePort
                                Data.encode
                                (Data.initClient auth)
                                (pageView auth Page.init)
                                (Firestore.init Data.decode)
                    in
                    ( SignedIn
                        { auth = auth
                        , page = Page.init
                        , firestore = firestore
                        , view = Maybe.withDefault (Document "Gaufre" []) mview
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
                        ( fs, mview, cmd ) =
                            Firestore.digest updatePort
                                Data.encode
                                (pageView r.auth r.page)
                                firestore
                    in
                    ( SignedIn
                        { r
                            | firestore = fs
                            , view = Maybe.withDefault r.view mview
                        }
                    , cmd
                    )

                Page m ->
                    case m of
                        Page.SignOut ->
                            ( NotSignedIn, signOut () )

                        _ ->
                            let
                                ( page, upd, cmd ) =
                                    Page.update r.auth m r.page

                                ( fs, mview, updcmd ) =
                                    Firestore.update
                                        updatePort
                                        Data.encode
                                        upd
                                        (pageView r.auth page)
                                        r.firestore
                            in
                            ( SignedIn
                                { r
                                    | page = page
                                    , firestore = fs
                                    , view = Maybe.withDefault r.view mview
                                }
                            , Cmd.batch [ updcmd, Cmd.map Page cmd]
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


appView : Model -> Document Msg
appView model =
    case model of
        NotSignedIn ->
            { title = "Gaufre"
            , body = [ Html.map (always SignIn) Entrance.view ]
            }

        SignedIn { auth, firestore, view, page } ->
            view


pageView : Auth -> Page.Page -> Data -> Access.Accessor (Document Msg)
pageView auth page data =
    Access.map
        (\{ title, body } ->
            { title = title, body = List.map (Html.map Page) body }
        )
    <|
        Page.view auth page data



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
        , view = appView
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
