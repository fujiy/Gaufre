module Data.Project exposing (..)

import Array
import Data exposing (..)
import Data.User as User
import Data.Work as Work
import Dict.Extra as Dict
import Firestore exposing (..)
import Firestore.Access as Access
import Firestore.Desc as Desc
import Firestore.Lens as Lens exposing (o, where_)
import Firestore.Path as Path
import Firestore.Path.Id as Id exposing (Id, unId)
import Firestore.Path.Id.Map as IdMap
import Firestore.Update as Update exposing (Updater)
import GDrive
import Maybe.Extra as Maybe
import Util exposing (flip)


type Role
    = Owner
    | Admin
    | Staff


type alias Authority =
    { manageMembers : Bool
    , manageWorkStaffs : Bool
    , editStructure : Bool
    , deleteProject : Bool
    }


staffAuthority : Authority
staffAuthority =
    Authority False False False False


adminAuthority : Authority
adminAuthority =
    { staffAuthority
        | manageMembers = True
        , manageWorkStaffs = True
        , editStructure = True
    }


ownerAuthority : Authority
ownerAuthority =
    { adminAuthority | deleteProject = True }


authority : Role -> Authority
authority role =
    case role of
        Owner ->
            ownerAuthority

        Admin ->
            adminAuthority

        Staff ->
            staffAuthority



-- Firestore


type alias Collection =
    Firestore.Collection ProjectSub Project


type alias Reference =
    Firestore.Reference ProjectSub Project


type alias Document =
    Firestore.Document ProjectSub Project



-- Lenses


deref : Lens Root Data Item Reference -> Lens Root Data Doc Document
deref =
    Lens.deref projects Lens.end


members : Project -> Lens Root Data Doc (List User.Document)
members p =
    Lens.derefs users Lens.end <|
        Lens.const p.members


ref : Id Project -> Reference
ref id =
    Firestore.ref <| Path.fromIds [ "projects", unId id ]


works : Lens Doc Document Col Work.Collection
works =
    Lens.subCollection .works (\b a -> { a | works = b })


work : Id Work -> Lens Doc Document Doc Work.Document
work id =
    o works <| Lens.doc id



-- Utilities


init : GDrive.FileMeta -> User.Reference -> Project
init folder user =
    { id = folder.id
    , name = folder.name
    , members = [ user ]
    , admins = [ user ]
    , owner = user
    , processes = IdMap.empty
    , parts = IdMap.empty
    }


userRole : Project -> Id User -> Role
userRole p id =
    if Firestore.getId p.owner == id then
        Owner

    else if List.any (Firestore.getId >> (==) id) p.admins then
        Admin

    else
        Staff


myRole : Project -> Auth -> Role
myRole p auth =
    userRole p <| Id.fromString auth.uid


makePartName : Int -> String
makePartName i =
    "カット" ++ String.fromInt i


newPart : Project -> ( Id Part, Part )
newPart p =
    let
        last =
            IdMap.items p.parts
                |> List.sortBy (.order >> negate)
                |> List.head
                |> Maybe.unwrap 0 .order

        try id name =
            if IdMap.member (Id.fromString <| String.fromInt id) p.parts then
                try (id + 1) name

            else if
                IdMap.any
                    (\part -> part.name == makePartName name)
                    p.parts
            then
                try id (name + 1)

            else
                ( Id.fromString <| String.fromInt id
                , { name = makePartName name
                  , order = last + 1
                  , parent = Nothing
                  }
                )
    in
    try (IdMap.size p.parts) (IdMap.size p.parts + 1)



-- Updaters


type Update
    = Join
    | Add GDrive.FileMeta
    | AddProcess Process GDrive.FileMeta
    | AddPart (List (Id Process)) (Id Part) Part
    | InviteMember String
    | SetProcessUpstreams Bool (Id Process) (List (Id Process))
    | WorkUpdate (List (Id Work)) Work.Update
    | None


update :
    Auth
    -> Id Project
    -> Update
    -> Updater Data Update
update auth projectId upd =
    let
        lens =
            project projectId
    in
    case upd of
        Join ->
            Update.modify (myClient auth) clientDesc <|
                \client ->
                    { client
                        | projects =
                            Array.push (ref projectId) client.projects
                    }

        Add folder ->
            Update.all
                [ Update.modify (myClient auth) clientDesc <|
                    \client ->
                        { client
                            | projects =
                                Array.push (ref projectId) client.projects
                        }
                , Update.set lens projectDesc <|
                    init folder (Data.myRef auth)
                , Update.batch <|
                    flip List.map Work.defaultProcesses <|
                        \process _ ->
                            GDrive.createFolder
                                auth.token
                                process.name
                                [ folder.id ]
                                |> Cmd.map
                                    (Result.map
                                        (AddProcess process)
                                        >> Result.withDefault None
                                    )
                ]

        AddProcess process folder ->
            Update.modify lens projectDesc <|
                \project ->
                    { project
                        | processes =
                            IdMap.insert (Id.fromString folder.id)
                                process
                                project.processes
                    }

        InviteMember name ->
            let
                email =
                    name ++ "@gmail.com"
            in
            Update.andThen
                (Access.access <|
                    (o users <|
                        o (where_ "email" Lens.EQ Desc.string email)
                            Lens.getAll
                    )
                )
                (\us ->
                    case us of
                        [ user ] ->
                            Update.all
                                [ Update.modify lens projectDesc <|
                                    \p ->
                                        { p
                                            | members =
                                                User.ref (Id.self user)
                                                    :: p.members
                                        }
                                , Update.command <|
                                    \_ ->
                                        GDrive.permissions_create auth.token
                                            (unId projectId)
                                            { role = GDrive.Writer
                                            , type_ = GDrive.User user.email
                                            }
                                            |> Cmd.map (\_ -> None)
                                ]

                        _ ->
                            Update.none
                )

        AddPart processes newId part ->
            Update.all
                [ Update.modify lens projectDesc <|
                    \p -> { p | parts = IdMap.insert newId part p.parts }
                , List.map
                    (\processId _ ->
                        GDrive.createFolder auth.token
                            part.name
                            [ unId processId ]
                            |> Cmd.map
                                (Result.map
                                    (Work.FolderCreated processId newId
                                        >> WorkUpdate [ Id.null ]
                                    )
                                    >> Result.withDefault None
                                )
                    )
                    processes
                    |> Update.batch
                ]

        SetProcessUpstreams updateExisting processId upstreams ->
            Update.all
                [ Update.modify lens projectDesc <|
                    \project ->
                        { project
                            | processes =
                                IdMap.modify processId
                                    (\process ->
                                        { process
                                            | upstreams =
                                                List.map unId upstreams
                                        }
                                    )
                                    project.processes
                        }
                , if updateExisting then
                    Update.andThen
                        (Access.access
                            (o lens <|
                                o works <|
                                    o (Work.processIs processId) Lens.getAll
                            )
                        )
                        (\ws ->
                            Update.for ws <|
                                \w ->
                                    Update.succeed <|
                                        WorkUpdate [ Id.self w ] <|
                                            Work.SetUpstreamProcesses upstreams
                        )

                  else
                    Update.none
                ]

        WorkUpdate ids wupd ->
            List.map
                (\workId ->
                    Work.update auth projectId workId (o lens works) wupd
                        |> Update.map (WorkUpdate [ workId ])
                )
                ids
                |> Update.all

        None ->
            Update.none
