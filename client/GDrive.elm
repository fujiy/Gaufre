module GDrive exposing (..)

import Http exposing (Body, Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Token =
    String


type alias Id =
    String


type alias FileMeta =
    { id : Id
    , name : String
    }


decodeFileMeta : Decoder FileMeta
decodeFileMeta =
    Decode.map2 FileMeta
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


folders : Token -> String -> Cmd (Result Error (List FileMeta))
folders token name =
    files_list token
        [ ( "q"
          , "mimeType='application/vnd.google-apps.folder' and "
                ++ "name contains '"
                ++ name
                ++ "'"
          )
        ]


createFolder : Token -> String -> List Id -> Cmd (Result Error FileMeta)
createFolder token name parents =
    request_ token
        "POST"
        "files"
        []
        (Encode.object
            [ ( "name", Encode.string name )
            , ( "mimeType", Encode.string "application/vnd.google-apps.folder" )
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


request :
    Token
    -> String
    -> String
    -> List ( String, String )
    -> Decoder a
    -> Cmd (Result Http.Error a)
request token method api options decoder =
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
