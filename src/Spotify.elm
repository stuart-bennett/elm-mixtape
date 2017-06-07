module Spotify exposing (search)
import Json.Decode as Decode
import Http exposing (..)
import String exposing (concat)
import Querystring exposing (..)

search : String -> String -> (Result Http.Error (List String) -> msg) -> Cmd msg
search searchTerm token fn =
    doApiRequest
        "/search"
        (Querystring.convert
            [ ("q", searchTerm)
            , ("type", "artist,track") ])
        token
        fn

searchResultDecoder : Decode.Decoder (List String)
searchResultDecoder =
    Decode.at["artists", "items"] (Decode.list (Decode.at["name"] Decode.string))

doApiRequest : String -> Querystring -> String -> (Result Http.Error (List String) -> msg) -> Cmd msg
doApiRequest endpoint querystring token fn =
    let
        absoluteUrl =
            "https://api.spotify.com/v1" ++
            endpoint ++
            "?" ++
            (Querystring.toString querystring)

        request = Http.request
            { method = "GET"
            , headers = [Http.header "Authorization" ("Bearer " ++ token)]
            , url = absoluteUrl
            , body = Http.emptyBody
            , expect = Http.expectJson searchResultDecoder
            , timeout = Nothing
            , withCredentials = False }
    in
        Http.send fn request
