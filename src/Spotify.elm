module Spotify exposing (search, fetchPlaylists, SearchResult, Playlist)
import Json.Decode as Decode
import Http exposing (..)
import String exposing (concat)
import Querystring exposing (..)

type SearchResultType = Artist
    | Track

type alias SearchResult =
    { name: String
    , id: String
    , type_: SearchResultType
    }

type alias Playlist =
    { name: String
    , id: String
    , tracks: (List String)
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
            , ("type", "artist,track")
            , ("market", "GB") ])
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
    let
        artistDecoder =
            Decode.at["artists", "items"]
                <| Decode.list
                <| Decode.map3 SearchResult
                ( Decode.field "name" Decode.string )
                ( Decode.field "id" Decode.string )
                ( Decode.field "type" (Decode.map searchResultTypeDecoder Decode.string) )
        trackDecoder =
            Decode.at["tracks", "items"]
                <| Decode.list
                <| Decode.map3 SearchResult
                ( Decode.field "name" Decode.string )
                ( Decode.field "id" Decode.string )
                ( Decode.field "type" (Decode.map searchResultTypeDecoder Decode.string) )
    in
        trackDecoder

searchResultTypeDecoder : String -> SearchResultType
searchResultTypeDecoder val =
    case val of
        "artist" -> Artist
        _ -> Track


playlistDecoder : Decode.Decoder (List Playlist)
playlistDecoder =
    Decode.at ["items"]
        <| Decode.list
        <| Decode.map3 Playlist
        ( Decode.field "name" Decode.string )
        ( Decode.field "id" Decode.string )
        ( Decode.map (\_ -> []) Decode.string )

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
