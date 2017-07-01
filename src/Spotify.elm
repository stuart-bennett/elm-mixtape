module Spotify exposing (search, fetchPlaylists, SearchResult, Playlist)
import Json.Decode as Decode
import Http exposing (..)
import String exposing (concat)
import Querystring exposing (..)

type alias SearchResult =
    { name: String
    , id: String
    }

type alias Playlist =
    { name: String
    , id: String
    }

fetchPlaylists : String -> (Result Http.Error (List Playlist) -> msg) -> Cmd msg
fetchPlaylists token fn =
    let
        endpoint = "/me/playlists"
    in
        doApiRequest
            endpoint
            Querystring.empty
            token
            fn
            playlistDecoder


search : String -> String -> (Result Http.Error (List SearchResult) -> msg) -> Cmd msg
search searchTerm token fn =
    let
        endpoint = "/search"
        querystring = (Querystring.convert
            [ ("q", searchTerm)
            , ("type", "artist,track") ])
    in
        doApiRequest
            endpoint
            querystring
            token
            fn
            searchResultDecoder

-- Decoders
searchResultDecoder : Decode.Decoder (List SearchResult)
searchResultDecoder =
    Decode.at["artists", "items"]
        <| Decode.list
        <| Decode.map2 SearchResult
        ( Decode.field "name" Decode.string )
        ( Decode.field "id" Decode.string )

playlistDecoder : Decode.Decoder (List Playlist)
playlistDecoder =
    Decode.at ["items"]
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
