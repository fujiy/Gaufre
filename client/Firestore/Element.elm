module Firestore.Element exposing (doc, doc_, firestore, get, get_, list, list_)

import Array
import Dict
import Element as El exposing (Attribute, Element)
import Firestore.Internal exposing (..)
import Html
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode
import Json.Encode


firestore : Firestore r -> (r -> Element msg) -> Element msg
firestore (Firestore d) view =
    view d.data


doc : Collection r -> Id -> (r -> Element msg) -> Element msg
doc col id view =
    doc_ col id <|
        \rr ->
            case rr of
                Failure ->
                    El.none

                Loading ->
                    El.none

                Committing r ->
                    view r

                UpToDate r ->
                    view r


doc_ :
    Collection r
    -> Id
    -> (Remote r -> Element msg)
    -> Element msg
doc_ (Collection { path, documents }) id view =
    requireData (Array.push id path) <|
        case Dict.get id documents of
            Nothing ->
                view Failure

            Just (Document d) ->
                view d.data


list : Collection r -> (List ( Id, r ) -> Element msg) -> Element msg
list col f =
    list_ col
        (List.filterMap
            (\( id, rr ) ->
                case rr of
                    Failure ->
                        Nothing

                    Loading ->
                        Nothing

                    Committing r ->
                        Just ( id, r )

                    UpToDate r ->
                        Just ( id, r )
            )
            >> f
        )


list_ :
    Collection r
    -> (List ( Id, Remote r ) -> Element msg)
    -> Element msg
list_ (Collection c) view =
    requireData c.path <|
        let
            docs =
                Dict.toList c.documents
                    |> List.filterMap
                        (\( id, Document d ) ->
                            case d.data of
                                Failure ->
                                    Nothing

                                _ ->
                                    Just ( id, d.data )
                        )
        in
        view docs


get : Reference r -> (r -> Element msg) -> Element msg
get ref view =
    get_ ref <|
        \rr ->
            case rr of
                Failure ->
                    El.none

                Loading ->
                    El.none

                Committing r ->
                    view r

                UpToDate r ->
                    view r


get_ :
    Reference r
    -> (Remote r -> Element msg)
    -> Element msg
get_ (Reference path (Document d)) view =
    requireReferencedData path d.path <| view d.data


requireData : Path -> Element msg -> Element msg
requireData path el =
    El.el
        [ El.behindContent <|
            El.html <|
                Html.node "data-requester"
                    [ Html.property "path" <| encodeRef path
                    ]
                    []
        ]
        el


requireReferencedData : Path -> Path -> Element msg -> Element msg
requireReferencedData path ref el =
    El.el
        [ El.behindContent <|
            El.html <|
                Html.node "data-requester"
                    [ Html.property "path" <| encodeRef path
                    , Html.property "reference" <| encodeRef ref
                    ]
                    []
        ]
        el


encodeRef : Path -> Json.Encode.Value
encodeRef =
    Json.Encode.array Json.Encode.string
