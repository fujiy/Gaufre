module Firestore.Path.Map exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Firestore.Path as Path exposing (Path, SomeId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe
import Util exposing (orWith)


type alias Paths =
    Map ()


type Map a
    = Root (Dict SomeId (Col a))


type Col a
    = Col (Maybe a) (Dict SomeId (Doc a)) (Dict Query (Col a))


type Doc a
    = Doc (Maybe a) (Dict SomeId (Col a))


type alias Query =
    ( String, String, String )


empty : Map a
empty =
    Root Dict.empty


emptyCol : Col a
emptyCol =
    Col Nothing Dict.empty Dict.empty


emptyDoc : Doc a
emptyDoc =
    Doc Nothing Dict.empty


getColRoot : Col a -> Maybe a
getColRoot (Col ma _ _) =
    ma


getDocRoot : Doc a -> Maybe a
getDocRoot (Doc ma _) =
    ma


colRootItem : a -> Col a
colRootItem a =
    Col (Just a) Dict.empty Dict.empty


docRootItem : a -> Doc a
docRootItem a =
    Doc (Just a) Dict.empty


singleton : Path -> a -> Map a
singleton path a =
    case path of
        Path.Root ->
            empty

        Path.RootCol id col ->
            Root <| Dict.singleton id <| singletonCol col a


singletonCol : Path.Col -> a -> Col a
singletonCol col a =
    case col of
        Path.Col ->
            Col (Just a) Dict.empty Dict.empty

        Path.SubDoc id doc ->
            Col Nothing (Dict.singleton id <| singletonDoc doc a) Dict.empty

        Path.Query field op value col_ ->
            Col Nothing
                Dict.empty
                (Dict.singleton
                    (queryKey field op value)
                    (singletonCol col_ a)
                )


queryKey : String -> String -> Value -> ( String, String, String )
queryKey field op value =
    ( field, op, Encode.encode 0 value )


singletonDoc : Path.Doc -> a -> Doc a
singletonDoc doc a =
    case doc of
        Path.Doc ->
            Doc (Just a) Dict.empty

        Path.SubCol id col ->
            Doc Nothing <| Dict.singleton id <| singletonCol col a


isEmpty : Map a -> Bool
isEmpty (Root d) =
    Dict.isEmpty d || not (Dict.any (\_ -> isEmptyCol >> not) d)


isEmptyCol : Col a -> Bool
isEmptyCol (Col ma d q) =
    (ma == Nothing)
        && (Dict.isEmpty d || not (Dict.any (\_ -> isEmptyDoc >> not) d))
        && (Dict.isEmpty q || not (Dict.any (\_ -> isEmptyCol >> not) q))


isEmptyDoc : Doc a -> Bool
isEmptyDoc (Doc ma d) =
    (ma == Nothing)
        && (Dict.isEmpty d || not (Dict.any (\_ -> isEmptyCol >> not) d))


at : SomeId -> Map a -> Col a
at id (Root d) =
    Dict.get id d |> Maybe.withDefault emptyCol


subs : Map a -> Dict SomeId (Col a)
subs (Root d) =
    d


subCols : Doc a -> Dict SomeId (Col a)
subCols (Doc _ d) =
    d


subQueries :
    Col a
    -> List { field : String, op : String, value : Value, col : Col a }
subQueries (Col _ _ q) =
    Dict.toList q
        |> List.filterMap
            (\( ( field, op, v ), col ) ->
                Decode.decodeString Decode.value v
                    |> Result.toMaybe
                    |> Maybe.map
                        (\value ->
                            { field = field, op = op, value = value, col = col }
                        )
            )


subDocs : Col a -> Dict SomeId (Doc a)
subDocs (Col _ d _) =
    d


insert : Path -> a -> Map a -> Map a
insert path a =
    update path (\_ -> Just a)


remove : Path -> Map a -> Map a
remove path =
    update path (\_ -> Nothing)


update : Path -> (Maybe a -> Maybe a) -> Map a -> Map a
update path f (Root d) =
    case path of
        Path.Root ->
            Root d

        Path.RootCol id colPath ->
            Root <|
                Dict.update id
                    (\mcol ->
                        case mcol of
                            Nothing ->
                                f Nothing
                                    |> Maybe.unwrap Nothing
                                        (\a -> Just <| singletonCol colPath a)

                            Just col ->
                                Just <| updateCol colPath f col
                    )
                    d


updateDoc : Path.Doc -> (Maybe a -> Maybe a) -> Doc a -> Doc a
updateDoc path f (Doc ma d) =
    case path of
        Path.Doc ->
            Doc (f ma) d

        Path.SubCol id colPath ->
            Doc ma <|
                Dict.update id
                    (\mcol ->
                        case mcol of
                            Nothing ->
                                f Nothing
                                    |> Maybe.unwrap Nothing
                                        (\a -> Just <| singletonCol colPath a)

                            Just col ->
                                Just <| updateCol colPath f col
                    )
                    d


updateCol : Path.Col -> (Maybe a -> Maybe a) -> Col a -> Col a
updateCol path f (Col ma d q) =
    case path of
        Path.Col ->
            Col (f ma) d q

        Path.SubDoc id docPath ->
            Col ma
                (Dict.update id
                    (\mdoc ->
                        case mdoc of
                            Nothing ->
                                f Nothing
                                    |> Maybe.unwrap Nothing
                                        (\a -> Just <| singletonDoc docPath a)

                            Just doc ->
                                Just <| updateDoc docPath f doc
                    )
                    d
                )
                q

        Path.Query field op value colPath ->
            Col ma
                d
                (Dict.update ( field, op, Encode.encode 0 value )
                    (\mcol ->
                        case mcol of
                            Nothing ->
                                f Nothing
                                    |> Maybe.unwrap Nothing
                                        (\a -> Just <| singletonCol colPath a)

                            Just col ->
                                Just <| updateCol colPath f col
                    )
                    q
                )


map : (a -> b) -> Map a -> Map b
map f (Root d) =
    Root (Dict.map (\_ -> mapCol f) d)


mapCol : (a -> b) -> Col a -> Col b
mapCol f (Col m d q) =
    Col (Maybe.map f m)
        (Dict.map (\_ -> mapDoc f) d)
        (Dict.map (\_ -> mapCol f) q)


mapDoc : (a -> b) -> Doc a -> Doc b
mapDoc f (Doc m d) =
    Doc (Maybe.map f m) (Dict.map (\_ -> mapCol f) d)


sub : SomeId -> Col a -> Map a
sub id col =
    Root <| Dict.singleton id col


toList : Map a -> List ( Path, a )
toList (Root d) =
    Dict.toList d
        |> List.concatMap
            (\( id, col ) ->
                toListCol col
                    |> List.map
                        (\( path, a ) -> ( Path.RootCol id path, a ))
            )


toListCol : Col a -> List ( Path.Col, a )
toListCol (Col ma d q) =
    Dict.toList d
        |> List.concatMap
            (\( id, doc ) ->
                toListDoc doc
                    |> List.map
                        (\( path, a ) -> ( Path.SubDoc id path, a ))
            )


toListDoc : Doc a -> List ( Path.Doc, a )
toListDoc (Doc ma d) =
    Dict.toList d
        |> List.concatMap
            (\( id, col ) ->
                toListCol col
                    |> List.map
                        (\( path, a ) -> ( Path.SubCol id path, a ))
            )


filterMap : (a -> Maybe b) -> Map a -> Map b
filterMap f (Root d) =
    Root <| Dict.map (\_ -> filterMapCol f) d


filterMapCol : (a -> Maybe b) -> Col a -> Col b
filterMapCol f (Col ma d q) =
    Col (Maybe.andThen f ma)
        (Dict.map (\_ -> filterMapDoc f) d)
        (Dict.map (\_ -> filterMapCol f) q)


filterMapDoc : (a -> Maybe b) -> Doc a -> Doc b
filterMapDoc f (Doc ma d) =
    Doc (Maybe.andThen f ma)
        (Dict.map (\_ -> filterMapCol f) d)


clean : Map a -> Map a
clean (Root rd) =
    let
        goCol (Col ma d q) =
            let
                d_ =
                    Dict.toList d
                        |> List.filterMap
                            (\( id, doc ) ->
                                goDoc doc |> Maybe.map (Tuple.pair id)
                            )
                        |> Dict.fromList

                q_ =
                    Dict.toList q
                        |> List.filterMap
                            (\( id, col ) ->
                                goCol col |> Maybe.map (Tuple.pair id)
                            )
                        |> Dict.fromList
            in
            if ma == Nothing && Dict.isEmpty d_ && Dict.isEmpty q_ then
                Nothing

            else
                Just <| Col ma d_ q_

        goDoc (Doc ma d) =
            let
                d_ =
                    Dict.toList d
                        |> List.filterMap
                            (\( id, col ) ->
                                goCol col |> Maybe.map (Tuple.pair id)
                            )
                        |> Dict.fromList
            in
            if ma == Nothing && Dict.isEmpty d_ then
                Nothing

            else
                Just <| Doc ma d_
    in
    Dict.toList rd
        |> List.filterMap
            (\( id, c ) -> goCol c |> Maybe.map (Tuple.pair id))
        |> Dict.fromList
        |> Root


merge : (a -> a -> a) -> Map a -> Map a -> Map a
merge f (Root da) (Root db) =
    Root <|
        Dict.merge Dict.insert
            (\id pl pr -> Dict.insert id <| mergeCol f pl pr)
            Dict.insert
            da
            db
            Dict.empty


mergeCol : (a -> a -> a) -> Col a -> Col a -> Col a
mergeCol f (Col ma da qa) (Col mb db qb) =
    Col
        (orWith f ma mb)
        (Dict.merge Dict.insert
            (\id pl pr -> Dict.insert id <| mergeDoc f pl pr)
            Dict.insert
            da
            db
            Dict.empty
        )
        (Dict.merge Dict.insert
            (\id pl pr -> Dict.insert id <| mergeCol f pl pr)
            Dict.insert
            qa
            qb
            Dict.empty
        )


mergeDoc : (a -> a -> a) -> Doc a -> Doc a -> Doc a
mergeDoc f (Doc ma da) (Doc mb db) =
    Doc
        (orWith f ma mb)
        (Dict.merge Dict.insert
            (\id pl pr -> Dict.insert id <| mergeCol f pl pr)
            Dict.insert
            da
            db
            Dict.empty
        )


diff : Map a -> Map a -> ( Map a, Map a )
diff (Root da) (Root db) =
    let
        ( dl_, dr_ ) =
            Dict.merge
                (\id p ( ql, qr ) -> ( Dict.insert id p ql, qr ))
                (\id pl pr ( ql, qr ) ->
                    let
                        ( sql, sqr ) =
                            diffCol pl pr
                    in
                    ( if isEmptyCol sql then
                        ql

                      else
                        Dict.insert id sql ql
                    , if isEmptyCol sqr then
                        qr

                      else
                        Dict.insert id sqr qr
                    )
                )
                (\id sm ( ql, qr ) -> ( ql, Dict.insert id sm qr ))
                da
                db
                ( Dict.empty, Dict.empty )
    in
    ( Root dl_, Root dr_ )


diffCol : Col a -> Col a -> ( Col a, Col a )
diffCol (Col ma da qa) (Col mb db qb) =
    let
        ( dl_, dr_ ) =
            Dict.merge
                (\id p ( dl, dr ) -> ( Dict.insert id p dl, dr ))
                (\id pl pr ( dl, dr ) ->
                    let
                        ( sdl, sdr ) =
                            diffDoc pl pr
                    in
                    ( if isEmptyDoc sdl then
                        dl

                      else
                        Dict.insert id sdl dl
                    , if isEmptyDoc sdr then
                        dr

                      else
                        Dict.insert id sdr dr
                    )
                )
                (\id sm ( dl, dr ) -> ( dl, Dict.insert id sm dr ))
                da
                db
                ( Dict.empty, Dict.empty )

        ( ql_, qr_ ) =
            Dict.merge
                (\id p ( ql, qr ) -> ( Dict.insert id p ql, qr ))
                (\id pl pr ( ql, qr ) ->
                    let
                        ( sql, sqr ) =
                            diffCol pl pr
                    in
                    ( if isEmptyCol sql then
                        ql

                      else
                        Dict.insert id sql ql
                    , if isEmptyCol sqr then
                        qr

                      else
                        Dict.insert id sqr qr
                    )
                )
                (\id sm ( ql, qr ) -> ( ql, Dict.insert id sm qr ))
                qa
                qb
                ( Dict.empty, Dict.empty )
    in
    if ma == mb then
        ( Col Nothing dl_ ql_, Col Nothing dr_ qr_ )

    else
        ( Col ma dl_ ql_, Col mb dr_ qr_ )


diffDoc : Doc a -> Doc a -> ( Doc a, Doc a )
diffDoc (Doc ma da) (Doc mb db) =
    let
        ( dl_, dr_ ) =
            Dict.merge
                (\id p ( ql, qr ) -> ( Dict.insert id p ql, qr ))
                (\id pl pr ( ql, qr ) ->
                    let
                        ( sql, sqr ) =
                            diffCol pl pr
                    in
                    ( if isEmptyCol sql then
                        ql

                      else
                        Dict.insert id sql ql
                    , if isEmptyCol sqr then
                        qr

                      else
                        Dict.insert id sqr qr
                    )
                )
                (\id sm ( ql, qr ) -> ( ql, Dict.insert id sm qr ))
                da
                db
                ( Dict.empty, Dict.empty )
    in
    if ma == mb then
        ( Doc Nothing dl_, Doc Nothing dr_ )

    else
        ( Doc ma dl_, Doc mb dr_ )



-- Json encoder and decoder


encode : (a -> Value) -> Map a -> Value
encode enc (Root d) =
    Encode.list (encodeCol enc) <| Dict.toList d


encodeCol : (a -> Value) -> ( SomeId, Col a ) -> Value
encodeCol enc ( id, Col ma d q ) =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "item", Maybe.unwrap Encode.null enc ma )
        , ( "sub", Encode.list (encodeDoc enc) <| Dict.toList d )
        , ( "q", Encode.list (encodeQuery enc) <| Dict.toList q )
        ]


encodeQuery : (a -> Value) -> ( Query, Col a ) -> Value
encodeQuery enc ( ( field, op, value ), Col ma d q ) =
    Encode.object
        [ ( "field", Encode.string field )
        , ( "op", Encode.string op )
        , ( "value"
          , Decode.decodeString Decode.value value
                |> Result.withDefault Encode.null
          )
        , ( "item", Maybe.unwrap Encode.null enc ma )
        , ( "sub", Encode.list (encodeDoc enc) <| Dict.toList d )
        , ( "q", Encode.list (encodeQuery enc) <| Dict.toList q )
        ]


encodeDoc : (a -> Value) -> ( SomeId, Doc a ) -> Value
encodeDoc enc ( id, Doc ma d ) =
    Encode.object
        [ ( "id", Encode.string id )
        , ( "item", Maybe.unwrap Encode.null enc ma )
        , ( "sub", Encode.list (encodeCol enc) <| Dict.toList d )
        ]


decode : Decoder a -> Decoder (Map a)
decode dec =
    Decode.list (decodeCol dec) |> Decode.map (Dict.fromList >> Root)


decodeCol : Decoder a -> Decoder ( SomeId, Col a )
decodeCol dec =
    Decode.lazy <|
        \_ ->
            Decode.map2 Tuple.pair
                (Decode.field "id" Decode.string)
            <|
                Decode.map3 Col
                    (Decode.field "item" <| Decode.nullable dec)
                    (Decode.field "sub" <|
                        Decode.map Dict.fromList <|
                            Decode.list <|
                                decodeDoc dec
                    )
                    (Decode.field "q" <|
                        Decode.map Dict.fromList <|
                            Decode.list <|
                                decodeQuery dec
                    )


decodeQuery : Decoder a -> Decoder ( Query, Col a )
decodeQuery dec =
    Decode.lazy <|
        \_ ->
            Decode.map2 Tuple.pair
                (Decode.map3
                    (\field op value -> ( field, op, Encode.encode 0 value ))
                    (Decode.field "field" Decode.string)
                    (Decode.field "op" Decode.string)
                    (Decode.field "value" Decode.value)
                )
            <|
                Decode.map3 Col
                    (Decode.field "item" <| Decode.nullable dec)
                    (Decode.field "sub" <|
                        Decode.map Dict.fromList <|
                            Decode.list <|
                                decodeDoc dec
                    )
                    (Decode.field "q" <|
                        Decode.map Dict.fromList <|
                            Decode.list <|
                                decodeQuery dec
                    )


decodeDoc : Decoder a -> Decoder ( SomeId, Doc a )
decodeDoc dec =
    Decode.map2 Tuple.pair
        (Decode.field "id" Decode.string)
    <|
        Decode.map2 Doc
            (Decode.field "item" <| Decode.nullable dec)
            (Decode.field "sub" <|
                Decode.map Dict.fromList <|
                    Decode.list <|
                        decodeCol dec
            )
