port module Main exposing (..)

import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Data exposing (Auth, Data, User)
import Data.Client as Client
import Firestore as Firestore exposing (Firestore)
import Firestore.Access as Access exposing (Accessor)
import Firestore.Update as Update
import Html exposing (..)
import Page
import Page.Entrance as Entrance
import Task
import Time
import Url exposing (Url)


port setLocalStorage : ( String, String ) -> Cmd msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port authorized : ({ auth : { uid : String, token : String }, user : { id : String, name : String, image : String, email : String } } -> msg) -> Sub msg


port firestoreSubPort : Firestore.SubPort msg


port firestoreCmdPort : Firestore.CmdPort msg


port newTab : String -> Cmd msg



-- Model -----------------------------------------------------------------------


type Model
    = NotSignedIn
        { navKey : Nav.Key
        , url : Url
        , zone : Time.Zone
        , now : Time.Posix
        }
    | SignedIn
        { auth : Data.Auth
        , page : Page.Model
        , firestore : Firestore Data.Data Msg
        , view : Document Msg
        , url : Url
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags origin navKey =
    ( NotSignedIn
        { navKey = navKey
        , url = origin
        , zone = Time.utc
        , now = Time.millisToPosix 0
        }
    , Cmd.batch
        [ Task.perform SetTimeZone Time.here
        , Task.perform Clock Time.now
        ]
    )



-- Update ----------------------------------------------------------------------


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | SignIn
    | SignOut
    | Authorized Auth User
    | SetTimeZone Time.Zone
    | Clock Time.Posix
    | Firestore (Firestore.FirestoreSub Data Msg)
    | Page Page.Msg
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotSignedIn m ->
            case msg of
                SignIn ->
                    ( model, signIn () )

                Authorized auth user ->
                    let
                        ( firestore, mview, cmd ) =
                            Firestore.update
                                firestoreCmdPort
                                (Client.init auth user
                                    |> Update.map (\_ -> None)
                                )
                                (pageView auth <| Page.init m.url)
                                (Firestore.init Data.desc)

                        page =
                            Page.init m.url
                    in
                    ( SignedIn
                        { auth = auth
                        , page = page
                        , firestore = firestore
                        , view =
                            Maybe.withDefault (Document "Gaufre" []) mview
                        , url = m.url
                        }
                    , Cmd.batch
                        [ cmd
                        , Page.initialize auth page |> Cmd.map Page
                        ]
                    )

                SetTimeZone zone ->
                    ( NotSignedIn { m | zone = zone }, Cmd.none )

                Clock t ->
                    ( NotSignedIn { m | now = t }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SignedIn r ->
            case msg of
                Authorized auth _ ->
                    let
                        ( fs, mview, cmd ) =
                            Firestore.update
                                firestoreCmdPort
                                Update.none
                                (pageView auth r.page)
                                r.firestore
                    in
                    ( SignedIn
                        { r
                            | auth = auth
                            , firestore = fs
                            , view = Maybe.withDefault r.view mview
                        }
                    , cmd
                    )

                SignOut ->
                    ( NotSignedIn
                        { navKey = r.auth.navKey
                        , url = r.url
                        , zone = r.auth.zone
                        , now = r.auth.now
                        }
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

                Page Page.SignOut ->
                    ( NotSignedIn
                        { navKey = r.auth.navKey
                        , url = r.url
                        , zone = r.auth.zone
                        , now = r.auth.now
                        }
                    , Cmd.batch
                        [ signOut (), Nav.load "/" ]
                    )

                Page m ->
                    let
                        ( page, upd ) =
                            Page.update r.auth m r.page

                        ( fs, mview, cmd ) =
                            Firestore.update
                                firestoreCmdPort
                                (Update.map Page upd)
                                (pageView r.auth page)
                                r.firestore
                    in
                    ( SignedIn
                        { r
                            | page = page
                            , firestore = fs
                            , view = Maybe.withDefault r.view mview
                        }
                    , cmd
                    )

                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model
                            , Nav.pushUrl r.auth.navKey (Url.toString url)
                            )

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
                    , Cmd.batch
                        [ Page.initialize r.auth page |> Cmd.map Page
                        , cmd
                        ]
                    )

                Clock t ->
                    let
                        auth_ =
                            r.auth

                        auth =
                            { auth_ | now = t }

                        ( fs, mview, cmd ) =
                            Firestore.update
                                firestoreCmdPort
                                Update.none
                                (pageView auth r.page)
                                r.firestore
                    in
                    ( SignedIn
                        { r
                            | auth = auth
                            , firestore = fs
                            , view = Maybe.withDefault r.view mview
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

        SignedIn { view } ->
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
        NotSignedIn m ->
            Sub.batch
                [ authorized <|
                    \{ auth, user } ->
                        Authorized
                            { uid = auth.uid
                            , token = auth.token
                            , navKey = m.navKey
                            , zone = m.zone
                            , now = m.now
                            }
                            { id = user.id
                            , name = user.name
                            , image = user.image
                            , email = user.email
                            , profile = ""
                            }
                , Time.every (60 * 1000) Clock
                ]

        SignedIn s ->
            Sub.batch
                [ authorized <|
                    \{ auth, user } ->
                        Authorized
                            { uid = auth.uid
                            , token = auth.token
                            , navKey = s.auth.navKey
                            , zone = s.auth.zone
                            , now = s.auth.now
                            }
                            { id = user.id
                            , name = user.name
                            , image = user.image
                            , email = user.email
                            , profile = ""
                            }
                , Firestore.watch firestoreSubPort s.firestore
                    |> Sub.map Firestore
                , Time.every (60 * 1000) Clock
                ]



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
