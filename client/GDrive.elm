module GDrive exposing (..)

import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)


type alias Token =
    String


type alias FileMeta =
    { id : String
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
        , body = Http.emptyBody
        , expect = Http.expectJson identity decoder
        , timeout = Nothing
        , tracker = Nothing
        }


endpoint : String
endpoint =
    "https://www.googleapis.com/drive/v3/"
