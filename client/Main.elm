port module Main exposing (..)

-- import Firestore.Access as Access

import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Data exposing (Auth, Data)
import Firestore as Firestore exposing (Firestore)
import Firestore.Access as Access exposing (Accessor)
import GDrive
import Html exposing (..)
import Page
import Page.Entrance as Entrance
import Url exposing (Url)


port setLocalStorage : ( String, String ) -> Cmd msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port authorized : ({ name : String, uid : String, token : String } -> msg) -> Sub msg


port firestoreSubPort : Firestore.SubPort msg


port firestoreCmdPort : Firestore.CmdPort msg


port newTab : String -> Cmd msg



-- Model -----------------------------------------------------------------------


type Model
    = NotSignedIn
        { navKey : Nav.Key
        , url : Url
        }
    | SignedIn
        { auth : Data.Auth
        , page : Page.Model
        , firestore : Firestore Data.Data
        , view : Document Msg
        , navKey : Nav.Key
        , url : Url
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags origin navKey =
    ( NotSignedIn { navKey = navKey, url = origin }, Cmd.none )



-- Update ----------------------------------------------------------------------


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | SignIn
    | SignOut
    | Authorized Auth
    | Firestore (Firestore.FirestoreSub Data)
    | Page Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotSignedIn { navKey, url } ->
            case msg of
                SignIn ->
                    ( model, signIn () )

                Authorized auth ->
                    let
                        ( firestore, mview, cmd ) =
                            Firestore.update
                                firestoreCmdPort
                                (Data.initClient auth)
                                (pageView auth <| Page.init url)
                                (Firestore.init Data.desc)
                    in
                    ( SignedIn
                        { auth = auth
                        , page = Page.init url
                        , firestore = firestore
                        , view = Maybe.withDefault (Document "Gaufre" []) mview
                        , navKey = navKey
                        , url = url
                        }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        SignedIn r ->
            case msg of
                SignOut ->
                    ( NotSignedIn { navKey = r.navKey, url = r.url }
                    , signOut ()
                    )

                Firestore sub ->
                    let
                        ( firestore, mview, cmd ) =
                            Firestore.apply firestoreCmdPort
                                (pageView r.auth r.page)
                                sub
                    in
                    ( SignedIn
                        { r
                            | firestore = firestore
                            , view = Maybe.withDefault r.view mview
                        }
                    , cmd
                    )

                Page m ->
                    case m of
                        Page.SignOut ->
                            ( NotSignedIn { navKey = r.navKey, url = r.url }
                            , signOut ()
                            )

                        _ ->
                            let
                                ( page, upd, cmd ) =
                                    Page.update r.auth m r.page

                                ( fs, mview, updcmd ) =
                                    Firestore.update
                                        firestoreCmdPort
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
                            , Cmd.batch [ updcmd, Cmd.map Page cmd ]
                            )

                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model, Nav.pushUrl r.navKey (Url.toString url) )

                        Browser.External "" ->
                            ( model, Cmd.none )

                        Browser.External href ->
                            ( model, newTab href )

                UrlChanged url ->
                    let
                        page =
                            Page.urlChanged r.page url

                        ( fs, mview, cmd ) =
                            Firestore.render
                                firestoreCmdPort
                                (pageView r.auth page)
                                r.firestore
                    in
                    ( SignedIn
                        { r
                            | page = page
                            , firestore = fs
                            , view = Maybe.withDefault r.view mview
                            , url = url
                        }
                    , cmd
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
        NotSignedIn _ ->
            { title = "Gaufre"
            , body = [ Html.map (always SignIn) Entrance.view ]
            }

        SignedIn { auth, firestore, view, page } ->
            view


pageView : Auth -> Page.Model -> Data -> Accessor Data (Document Msg)
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
        NotSignedIn _ ->
            authorized Authorized

        SignedIn { firestore } ->
            Firestore.watch firestoreSubPort firestore
                |> Sub.map Firestore



-- Main ------------------------------------------------------------------------


main : Program () Model Msg
main =
    application
        { init = init
        , view = appView
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
