module Util exposing (..)

import Array exposing (Array)
import Html exposing (Attribute)
import Html.Attributes exposing (attribute, class)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


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
