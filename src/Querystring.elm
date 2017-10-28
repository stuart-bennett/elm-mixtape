port module Querystring exposing (
      convert
    , empty
    , fromString
    , getValue
    , toString
    , Querystring )

import Http exposing (encodeUri)
import Maybe exposing(withDefault)

port getParam : String -> Cmd msg
port getParamResult : (String -> msg) -> Sub msg

type alias Querystring =
    List (String, Maybe String)

empty : Querystring
empty = []

{-| Convert a string like key1=value&key2=value2
into a structure equivalent to a list of tuples
e.g. [{"key1", "value1"}.{"key2","value2"}]
-}
fromString : String -> Querystring
fromString wholeFragment =
    String.split "&" wholeFragment
        |> List.map parseKeyValuePair

toString : List(String, Maybe String) -> String
toString list =
    String.join "&" <| List.map formatTuple list

{-| Get the value for a given key from querystring
    getValue "name"  "[("name", "name.value")] == Just "name.value"
    getValue "not-present" "[("name", "name.value")] == Nothing
-}
getValue : String -> Querystring -> Maybe String
getValue key q =
    let
        match = List.head
            <| List.filter (\kv -> (Tuple.first kv) == key) q
    in
        case match of
            Nothing -> Nothing
            Just m -> Tuple.second m

formatTuple : (String, Maybe String) -> String
formatTuple tuple =
    String.concat
        [ Tuple.first tuple
        , "="
        , withDefault "" <| Tuple.second tuple ]

convert : List (String, String) -> Querystring
convert list =
    List.map convertTuple
        <| List.map encodeTuple list

encodeTuple : (String, String) -> (String, String)
encodeTuple item =
    Tuple.mapFirst encodeUri
        <| Tuple.mapSecond encodeUri item

convertTuple : (String, String) -> (String, Maybe String)
convertTuple item =
        item |> Tuple.mapSecond Just

{-| Convert a string like key1=value1 into a tuple of
("key1", "value1). Note that a value isn't required

-}
parseKeyValuePair : String -> (String, Maybe String)
parseKeyValuePair nameAndValue =
    nameAndValue
        |> String.split "="
        |> intoTuple

intoTuple : List(String) -> (String, Maybe String)
intoTuple pair =
    let
        first = Maybe.withDefault "" <| List.head pair
        second = List.head
            <| Maybe.withDefault []
            <| List.tail pair
    in
        (first, second)

