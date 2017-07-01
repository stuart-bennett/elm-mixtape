port module Querystring exposing 
    ( convert
    , empty
    , getItems
    , toString
    , Querystring )

import Http exposing (encodeUri)
import Maybe exposing(withDefault)

port getParam : String -> Cmd msg
port getParamResult : (String -> msg) -> Sub msg

type alias Querystring =
    List (Maybe String, Maybe String)

empty : Querystring
empty = []

toString : List(Maybe String, Maybe String) -> String
toString list =
    String.join "&" <| List.map formatTuple list

formatTuple : (Maybe String, Maybe String) -> String
formatTuple tuple =
    String.concat
        [ withDefault "" <| Tuple.first tuple
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

convertTuple : (String, String) -> (Maybe String, Maybe String)
convertTuple item =
    Tuple.mapFirst Just
        <| Tuple.mapSecond Just item

getItems : String -> Querystring
getItems wholeFragment =
    String.split "&" wholeFragment
        |> List.map getItem

getItem : String -> (Maybe String, Maybe String)
getItem nameAndValue =
    String.split "=" nameAndValue
        |> intoTuple

intoTuple : List(String) -> (Maybe String, Maybe String)
intoTuple pair =
    let
        first = List.head pair
        second = List.head
            <| Maybe.withDefault []
            <| List.tail pair
    in
        (first, second)

