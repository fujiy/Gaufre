module GDrive exposing (..)

import Http exposing (Body, Error)
import Iso8601
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline as Decode exposing (optional, required)
import Json.Encode as Encode
import String
import Time


type alias Token =
    String


type alias Id =
    String


type alias FileMeta =
    { id : Id
    , name : String
    , mimeType : String
    , parents : List Id
    , webViewLink : String
    , iconLink : String
    , hasThumbnail : Bool
    , thumbnailLink : String
    , createdTime : Time.Posix
    , modifiedTime : Time.Posix
    }


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
        |> required "webViewLink" string
        |> required "iconLink" string
        |> required "hasThumbnail" bool
        |> optional "thumbnailLink" string ""
        |> required "createdTime" Iso8601.decoder
        |> required "modifiedTime" Iso8601.decoder


fields : List String
fields =
    [ "id"
    , "name"
    , "mimeType"
    , "parents"
    , "webViewLink"
    , "iconLink"
    , "hasThumbnail"
    , "thumbnailLink"
    , "createdTime"
    , "modifiedTime"
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
        [ ( "q"
          , "mimeType='application/vnd.google-apps.folder' and "
                ++ "name contains '"
                ++ name
                ++ "'"
          )
        , ( "fields", filesFields )
        ]


files : Token -> Id -> Cmd (Result Error (List FileMeta))
files token parent =
    files_list token
        [ ( "q", "'" ++ parent ++ "' in parents" )
        , ( "fields", filesFields )
        ]


createFolder : Token -> String -> List Id -> Cmd (Result Error FileMeta)
createFolder token name parents =
    request_ token
        "POST"
        "files"
        []
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
    -> List ( String, String )
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
        [ ( "fields", fileFields ) ]
        decodeFileMeta


request :
    Token
    -> String
    -> String
    -> List ( String, String )
    -> Decoder a
    -> Cmd (Result Http.Error a)
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
    -> List ( String, String )
    -> Body
    -> Decoder a
    -> Cmd (Result Http.Error a)
request_ token method api options body decoder =
    Http.request
        { method = method
        , headers =
            [ Http.header "Authorization" <| "Bearer " ++ token
            , Http.header "Accept" "application/json"
            ]
        , url =
            endpoint
                ++ api
                ++ "?"
                ++ List.foldr
                    (\( name, value ) query ->
                        name ++ "=" ++ value ++ "&" ++ query
                    )
                    ""
                    options
        , body = body
        , expect = Http.expectJson identity decoder
        , timeout = Nothing
        , tracker = Nothing
        }


endpoint : String
endpoint =
    "https://www.googleapis.com/drive/v3/"
