module Spotify exposing (search, Msg)
import Http exposing (..)
import Json.Decode as Decode

type Msg =
    Search

search : String -> String -> (Result Http.Error (List String) -> msg) -> Cmd msg
search searchTerm token fn =
    doApiRequest
        (String.concat ["/search?q=", searchTerm, "&type=artist,track"])
        token
        fn

searchResultDecoder : Decode.Decoder (List String)
searchResultDecoder =
    Decode.at["artists", "items"] (Decode.list (Decode.at["name"] Decode.string))

doApiRequest : String -> String -> (Result Http.Error (List String) -> msg) -> Cmd msg
doApiRequest endpoint token fn =
    let
        absoluteUrl = "https://api.spotify.com/v1" ++ endpoint
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
