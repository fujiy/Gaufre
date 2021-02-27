module Util exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Html exposing (Attribute)
import Html.Attributes exposing (attribute, class)
import Html.Events as Events exposing (stopPropagationOn)
import Json.Decode as Decode
import List


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


flip2 : (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a =
    f a b c


orWith : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
orWith f mx my =
    case ( mx, my ) of
        ( Nothing, _ ) ->
            my

        ( _, Nothing ) ->
            mx

        ( Just x, Just y ) ->
            Just <| f x y


singleton : a -> Array a
singleton a =
    Array.fromList [ a ]


uncons : Array a -> Maybe ( a, Array a )
uncons arr =
    let
        ( ls, rs ) =
            Array.splitAt 1 arr
    in
    Array.get 0 ls
        |> Maybe.map
            (\a -> ( a, rs ))


boolAttr : String -> Bool -> Attribute msg
boolAttr name b =
    attribute name <|
        if b then
            "true"

        else
            ""


classIf : Bool -> String -> Attribute msg
classIf cond cls =
    if cond then
        class cls

    else
        class ""


onMouseDownStop : msg -> Attribute msg
onMouseDownStop msg =
    stopPropagationOn "mousedown" <| Decode.succeed ( msg, True )


onDragEnter : msg -> Attribute msg
onDragEnter msg =
    Events.on "mouseenter" <|
        Decode.andThen
            (\buttons ->
                if buttons == 1 then
                    Decode.succeed msg

                else
                    Decode.fail "button unpressed"
            )
        <|
            Decode.field "buttons" Decode.int


values : (String -> Maybe a) -> Decode.Decoder (List a)
values f =
    Decode.string
        |> Decode.map
            (String.split "," >> List.filterMap f)


onChangeValues : Attribute (List String)
onChangeValues =
    Events.on "change" <|
        Decode.map
            (\s ->
                if s == "" then
                    []

                else
                    String.split "," s
            )
            Events.targetValue
