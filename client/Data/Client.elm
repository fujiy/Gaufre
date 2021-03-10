module Data.Client exposing (..)

import Array exposing (Array)
import Data exposing (..)
import Data.Project as Project
import Data.User as User
import Firestore exposing (..)
import Firestore.Desc as Desc exposing (DocumentDesc)
import Firestore.Lens as Lens exposing (o)
import Firestore.Update as Update exposing (Updater)


type alias Collection =
    Firestore.Collection () Client


type alias Document =
    Firestore.Document () Client



-- Utilities


type TaskState
    = Working
    | Reviewing
    | WaitingUpstream
    | WaitingSubmit
    | WaitingReview
    | Complete


defaultList : List TaskState
defaultList =
    [ WaitingUpstream
    , Working
    , WaitingSubmit
    , Reviewing
    , WaitingReview
    , Complete
    ]


taskTitle : TaskState -> String
taskTitle task =
    case task of
        Working ->
            "作業中"

        Reviewing ->
            "チェック中"

        WaitingUpstream ->
            "前工程待ち"

        WaitingSubmit ->
            "提出待ち"

        WaitingReview ->
            "チェック待ち"

        Complete ->
            "完了"


taskIconClass : TaskState -> String
taskIconClass task =
    case task of
        Working ->
            "paint brush"

        Reviewing ->
            "eye"

        WaitingUpstream ->
            "hourglass outline"

        WaitingSubmit ->
            "hourglass start"

        WaitingReview ->
            "hourglass half"

        Complete ->
            "check"



-- Lenses


projects : Lens Item Client Item (Array Project.Reference)
projects =
    Lens.lens .projects (\ps c -> { c | projects = ps })


my : Auth -> Lens Root Data Doc Document
my =
    myClient


currentProject : Auth -> Int -> Lens Root Data Doc Project.Document
currentProject auth i =
    Project.deref
        (o (my auth) <| o Lens.get <| o projects <| Lens.atArray i)



-- Updaters


init : Auth -> User -> Updater Data ()
init auth user =
    Update.all
        [ Update.map (\_ -> ()) <|
            Update.default (User.me auth) userDesc user
        , Update.map (\_ -> ()) <|
            Update.default (my auth) clientDesc <|
                { projects = Array.empty }
        ]
