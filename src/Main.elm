module Main exposing (..)

import Browser exposing (Document, application)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Url exposing (Url)


-- Model -----------------------------------------------------------------------

type Model = Home


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey = (Home, Cmd.none)


-- View ------------------------------------------------------------------------

view : Model -> Document Msg
view model =
    { title = "Gaufre"
    , body = []
    }


-- Update ----------------------------------------------------------------------

type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest

update : Msg -> Model -> ( Model, Cmd Msg )
update  msg model =
    ( model, Cmd.none )


-- Subscriptions ---------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none



-- Main ------------------------------------------------------------------------

main : Program Value Model Msg
main =
    application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ClickedLink
    , onUrlChange = ChangedUrl
    }
