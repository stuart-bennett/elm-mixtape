module Spotify exposing (search, SearchResult)
import Json.Decode as Decode
import Http exposing (..)
import String exposing (concat)
import Querystring exposing (..)

type alias SearchResult =
    { name: String
    , id: String
    }

search : String -> String -> (Result Http.Error (List SearchResult) -> msg) -> Cmd msg
search searchTerm token fn =
    doApiRequest
        "/search"
        (Querystring.convert
            [ ("q", searchTerm)
            , ("type", "artist,track") ])
        token
        fn
        searchResultDecoder

searchResultDecoder : Decode.Decoder (List SearchResult)
searchResultDecoder =
    Decode.at["artists", "items"] 
        <| Decode.list
        <| Decode.map2 SearchResult
        ( Decode.field "name" Decode.string )
        ( Decode.field "id" Decode.string )


doApiRequest : String -> Querystring -> String -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
doApiRequest endpoint querystring token fn decoder =
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
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False }
    in
        Http.send fn request
