port module Querystring exposing (..)

port getParam : String -> Cmd msg
port getParamResult : (String -> msg) -> Sub msg

getItems : String -> (List (Maybe String, Maybe String))
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
