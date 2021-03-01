module GDrive exposing (..)

import Bytes exposing (Bytes)
import Bytes.Decode as Bytes
import Dict exposing (Dict)
import File.Download
import Http exposing (Body, Error)
import Iso8601
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline as Decode exposing (optional, required)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import String
import Task exposing (Task)
import Time
import Url.Builder as Url
import Zip exposing (Zip)
import Zip.Entry


type alias Token =
    String


type alias Id =
    String


type alias FileMeta =
    { id : Id
    , name : String
    , mimeType : String
    , parents : List Id
    , webContentLink : String
    , webViewLink : String
    , iconLink : String
    , hasThumbnail : Bool
    , thumbnailLink : String
    , createdTime : Time.Posix
    , modifiedTime : Time.Posix
    , size : Int
    }


type alias Cache =
    { files : Dict Id FileMeta }


mimeType =
    { folder = "application/vnd.google-apps.folder" }


isFolder : FileMeta -> Bool
isFolder file =
    file.mimeType == mimeType.folder


decodeFileMeta : Decoder FileMeta
decodeFileMeta =
    Decode.succeed FileMeta
        |> required "id" string
        |> required "name" string
        |> required "mimeType" string
        |> required "parents" (Decode.list string)
        |> optional "webContentLink" string ""
        |> required "webViewLink" string
        |> required "iconLink" string
        |> required "hasThumbnail" bool
        |> optional "thumbnailLink" string ""
        |> required "createdTime" Iso8601.decoder
        |> required "modifiedTime" Iso8601.decoder
        |> optional "size" intString 0


intString : Decoder Int
intString =
    string
        |> Decode.andThen
            (String.toInt
                >> Maybe.unwrap
                    (Decode.fail "cannnot convert into an int")
                    Decode.succeed
            )


fields : List String
fields =
    [ "id"
    , "name"
    , "mimeType"
    , "parents"
    , "webViewLink"
    , "webContentLink"
    , "iconLink"
    , "hasThumbnail"
    , "thumbnailLink"
    , "createdTime"
    , "modifiedTime"
    , "size"
    ]


fileFields : String
fileFields =
    fields |> String.join ","


filesFields : String
filesFields =
    fields
        |> String.join ",files/"
        |> String.append "files/"


folders : Token -> String -> Cmd (Result Error (List FileMeta))
folders token name =
    files_list token
        [ Url.string "q" <|
            "mimeType='application/vnd.google-apps.folder' and "
                ++ "name contains '"
                ++ name
                ++ "'"
        , Url.string "fields" <| filesFields
        ]


filesIn : Token -> Id -> Cmd (Result Error (List FileMeta))
filesIn token parent =
    files_list token
        [ Url.string "q" <| "'" ++ parent ++ "' in parents"
        , Url.string "fields" <| filesFields
        ]


filesInTask : Token -> Id -> Task Error (List FileMeta)
filesInTask token parent =
    requestTask token
        "GET"
        "files"
        [ Url.string "q" <| "'" ++ parent ++ "' in parents"
        , Url.string "fields" <| filesFields
        ]
        Http.emptyBody
    <|
        Decode.field "files" <|
            Decode.list decodeFileMeta


createFolder : Token -> String -> List Id -> Cmd (Result Error FileMeta)
createFolder token name parents =
    request_ token
        "POST"
        "files"
        [ Url.string "fields" <| fileFields ]
        (Encode.object
            [ ( "name", Encode.string name )
            , ( "mimeType", Encode.string mimeType.folder )
            , ( "parents", Encode.list Encode.string parents )
            ]
            |> Http.jsonBody
        )
        decodeFileMeta


files_list :
    Token
    -> List Url.QueryParameter
    -> Cmd (Result Error (List FileMeta))
files_list token options =
    request token "GET" "files" options <|
        Decode.field "files" <|
            Decode.list decodeFileMeta


files_get : Token -> String -> Cmd (Result Error FileMeta)
files_get token id =
    request token
        "GET"
        ("files/" ++ id)
        [ Url.string "fields" fileFields ]
        decodeFileMeta


getData : Token -> FileMeta -> Cmd (Result Error Bytes)
getData token file =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" <| "Bearer " ++ token
            , Http.header "Accept" "application/json"
            ]
        , url =
            Url.crossOrigin endpoint
                [ "files", file.id ]
                [ Url.string "alt" "media" ]
        , body = Http.emptyBody
        , expect =
            Http.expectBytes identity
                (Bytes.bytes file.size)
        , timeout = Nothing
        , tracker = Nothing
        }


getDataTask : Token -> FileMeta -> Task Error Bytes
getDataTask token file =
    Http.task
        { method = "GET"
        , headers =
            [ Http.header "Authorization" <| "Bearer " ++ token
            , Http.header "Accept" "application/json"
            ]
        , url =
            Url.crossOrigin endpoint
                [ "files", file.id ]
                [ Url.string "alt" "media" ]
        , body = Http.emptyBody
        , resolver =
            Http.bytesResolver <|
                \response ->
                    case response of
                        Http.BadUrl_ s ->
                            Err <| Http.BadUrl s

                        Http.Timeout_ ->
                            Err <| Http.Timeout

                        Http.NetworkError_ ->
                            Err <| Http.NetworkError

                        Http.BadStatus_ m _ ->
                            Err <| Http.BadStatus m.statusCode

                        Http.GoodStatus_ _ bytes ->
                            Ok bytes
        , timeout = Nothing
        }


request :
    Token
    -> String
    -> String
    -> List Url.QueryParameter
    -> Decoder a
    -> Cmd (Result Error a)
request token method api options decoder =
    Cmd.map
        (Result.mapError <|
            Debug.log <|
                "GDrive Error: "
                    ++ method
                    ++ " "
                    ++ api
                    ++ Debug.toString options
        )
    <|
        request_ token method api options Http.emptyBody decoder


request_ :
    Token
    -> String
    -> String
    -> List Url.QueryParameter
    -> Body
    -> Decoder a
    -> Cmd (Result Error a)
request_ token method api options body decoder =
    Http.request
        { method = method
        , headers =
            [ Http.header "Authorization" <| "Bearer " ++ token
            , Http.header "Accept" "application/json"
            ]
        , url = Url.crossOrigin endpoint [ api ] options
        , body = body
        , expect = Http.expectJson identity decoder
        , timeout = Nothing
        , tracker = Nothing
        }


requestTask :
    Token
    -> String
    -> String
    -> List Url.QueryParameter
    -> Body
    -> Decoder a
    -> Task Error a
requestTask token method api options body decoder =
    Http.task
        { method = method
        , headers =
            [ Http.header "Authorization" <| "Bearer " ++ token
            , Http.header "Accept" "application/json"
            ]
        , url = Url.crossOrigin endpoint [ api ] options
        , body = body
        , resolver =
            Http.stringResolver <|
                \response ->
                    case response of
                        Http.BadUrl_ s ->
                            Err <| Http.BadUrl s

                        Http.Timeout_ ->
                            Err <| Http.Timeout

                        Http.NetworkError_ ->
                            Err <| Http.NetworkError

                        Http.BadStatus_ m _ ->
                            Err <| Http.BadStatus m.statusCode

                        Http.GoodStatus_ _ str ->
                            Decode.decodeString decoder str
                                |> Result.mapError
                                    (Decode.errorToString >> Http.BadBody)
        , timeout = Nothing
        }


endpoint : String
endpoint =
    "https://www.googleapis.com/drive/v3"


getZip : Token -> FileMeta -> Cmd (Result Error Bytes)
getZip token file_ =
    let
        go : String -> FileMeta -> Zip -> Task Error Zip
        go path file zip =
            let
                meta =
                    makeZipMeta path file
            in
            if isFolder file then
                filesInTask token file.id
                    |> Task.andThen
                        (List.foldr
                            (Task.andThen << go meta.path)
                            (Task.succeed <|
                                Zip.insert
                                    (Zip.Entry.createDirectory meta)
                                    zip
                            )
                        )

            else
                getDataTask token file
                    |> Task.map
                        (\bytes -> Zip.insert (Zip.Entry.store meta bytes) zip)
    in
    go "" file_ Zip.empty
        |> Task.attempt (Result.map Zip.toBytes)


makeZipMeta : String -> FileMeta -> Zip.Entry.Meta
makeZipMeta path file =
    { path = path ++ "/" ++ file.name
    , lastModified = ( Time.utc, file.modifiedTime )
    , comment = Nothing
    }


download : FileMeta -> Bytes -> Cmd msg
download file =
    File.Download.bytes file.name <|
        if isFolder file then
            "application/zip"

        else
            file.mimeType


flip f a b =
    f b a
