module Firestore.Decode exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Firestore.Internal as Internal exposing (..)
import Firestore.Types exposing (..)
import Json.Decode as Json
import Maybe.Extra as Maybe
import Result
import Util exposing (..)


type Decoder a
    = Decoder (Json.Decoder a)


decode : Decoder (Firestore r) -> Value (Firestore r) -> Firestore r
decode (Decoder dec) (Value v) =
    case Json.decodeValue dec v of
        Err err ->
            Debug.todo <| Json.errorToString err

        Ok fs ->
            fs


unDecoder : Decoder a -> Json.Decoder a
unDecoder (Decoder d) =
    d


firestore : r -> Decoder (Firestore r)
firestore r =
    Decoder <| Json.succeed <| Firestore { data = r, laters = noUpdater }


collection_ : Decoder (Document r) -> Decoder (Collection r)
collection_ d =
    Decoder <|
        Json.map2
            (\p ds -> Collection { path = p, documents = ds })
            (Json.field "path" <| unDecoder path)
            (Json.field "documents" <| Json.dict <| unDecoder d)


collection :
    Id
    -> Decoder (Document a)
    -> Decoder (Firestore (Collection a -> r))
    -> Decoder (Firestore r)
collection id d =
    Decoder
        << Json.map2
            (\mc (Firestore f) ->
                let
                    c =
                        Maybe.withDefault
                            (Collection
                                { path = topLevel id
                                , documents = Dict.empty
                                }
                            )
                            mc
                in
                Firestore { data = f.data c, laters = noUpdater }
            )
            (Json.maybe <| Json.field id <| unDecoder <| collection_ d)
        << unDecoder


subCollection :
    Id
    -> Decoder (Document a)
    -> Decoder (Document (Collection a -> r))
    -> Decoder (Document r)
subCollection id d =
    field id <| collection_ d


field :
    String
    -> Decoder a
    -> Decoder (Document (a -> r))
    -> Decoder (Document r)
field name d =
    Decoder
        << Json.map2
            (\m (Document df) ->
                Document
                    { path = df.path
                    , data =
                        case m of
                            Nothing ->
                                case df.data of
                                    Loading ->
                                        Loading

                                    _ ->
                                        Failure

                            Just r ->
                                mapRemote (\f -> f r) df.data
                    }
            )
            (Json.maybe <| Json.at [ "data", name ] <| unDecoder d)
        << unDecoder


document : r -> Decoder (Document r)
document r =
    Decoder <|
        Json.map2
            (\p status ->
                Document
                    { path = p
                    , data =
                        case status of
                            "loading" ->
                                Loading

                            "committing" ->
                                Committing r

                            "uptodate" ->
                                UpToDate r

                            _ ->
                                Failure
                    }
            )
            (Json.field "path" <| unDecoder path)
            (Json.field "status" Json.string)


mapDocument : (r -> a) -> Decoder (Document r) -> Decoder (Document a)
mapDocument f dec =
    map (Internal.mapDocument f) dec


reference : Decoder (Document r) -> Decoder (Reference r)
reference dec =
    Decoder <|
        flip Json.map Json.value <|
            \v ->
                Reference <|
                    \_ ->
                        case Json.decodeValue (unDecoder dec) v of
                            Ok d ->
                                d

                            Err e ->
                                Debug.todo "ERR" e



-- Document {path = root, data = Failure}
-- Json.lazy <|
--     \_ -> Json.map (\d -> Reference <| \_ -> d) <| unDecoder dec


path : Decoder Path
path =
    Decoder <| Json.array Json.string


string : Decoder String
string =
    Decoder Json.string


bool : Decoder Bool
bool =
    Decoder Json.bool


int : Decoder Int
int =
    Decoder Json.int


float : Decoder Float
float =
    Decoder Json.float


list : Decoder a -> Decoder (List a)
list =
    Decoder << Json.list << unDecoder


array : Decoder a -> Decoder (Array a)
array =
    Decoder << Json.array << unDecoder


dict : Decoder a -> Decoder (Dict String a)
dict =
    Decoder << Json.dict << unDecoder


optional : Decoder a -> Decoder (Maybe a)
optional =
    Decoder << Json.nullable << unDecoder


null : a -> Decoder a
null =
    Decoder << Json.null


lazy : (() -> Decoder a) -> Decoder a
lazy dec =
    Decoder <| Json.lazy <| dec >> unDecoder


map : (a -> b) -> Decoder a -> Decoder b
map f =
    Decoder << Json.map f << unDecoder
