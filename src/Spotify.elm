module Spotify exposing (
    search,
    getPlaylistTracks,
    savePlaylist,
    fetchPlaylists,
    savePlaylistTracks,
    SearchResult,
    Playlist,
    PlaylistTrack)

import Json.Decode as Decode
import Json.Encode as Encode
import Http exposing (..)
import String exposing (concat)
import Querystring exposing (..)

type SearchResultType = Artist
    | Track

type alias PlaylistTrack =
    { title : String
    , uri : String
    , isNew : Bool }

type alias SearchResult =
    { name : String
    , id : String
    , type_ : SearchResultType
    , uri : String
    }

type alias Playlist =
    { name: String
    , id: String
    }

savePlaylist : String -> Playlist -> (Result Http.Error Playlist -> msg) -> Cmd msg
savePlaylist token playlist fn =
    let
        endpoint = "/users/stu.bennett/playlists"
    in
        doApiRequest
            "POST"
            (Http.stringBody "application/json" "{ \"name\": \"testing\" }")
            endpoint
            Querystring.empty
            token
            fn
            playlistDecoder

fetchPlaylists : String -> (Result Http.Error (List Playlist) -> msg) -> Cmd msg
fetchPlaylists token fn =
    let
        endpoint = "/me/playlists"
    in
        doApiRequest
            "GET"
            Http.emptyBody
            endpoint
            Querystring.empty
            token
            fn
            playlistsDecoder

savePlaylistTracks : String -> String -> (List String) -> (Result Http.Error String -> msg) -> Cmd msg
savePlaylistTracks token playlistId trackUris fn =
    let
        endpoint =
            "/users/stu.bennett/playlists/" ++
            playlistId ++
            "/tracks"

        body = ( Http.stringBody
            "application/json"
            ( Encode.encode 0 ( tracklistEncoder trackUris ) ) )
    in
        doApiRequest
            "POST"
            body
            endpoint
            Querystring.empty
            token
            fn
            ( Decode.field "snapshot_id" Decode.string )

tracklistEncoder : (List String) -> Encode.Value
tracklistEncoder tracklist =
    Encode.object
    [ ("uris", Encode.list ( List.map Encode.string tracklist ) ) ]

getPlaylistTracks : String -> String -> (Result Http.Error (List PlaylistTrack) -> msg) -> Cmd msg
getPlaylistTracks token playlistId fn =
    let
        endpoint =
            "/users/stu.bennett/playlists/" ++
            playlistId ++
            "/tracks"
    in
        doApiRequest
            "GET"
            Http.emptyBody
            endpoint
            Querystring.empty
            token
            fn
            tracklistDecoder

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
            "GET"
            Http.emptyBody
            endpoint
            querystring
            token
            fn
            searchResultDecoder

-- Decoders
tracklistDecoder : Decode.Decoder (List PlaylistTrack)
tracklistDecoder =
    Decode.at["items"]
        <| Decode.list
        <| Decode.map2
        ( \x y -> { title = x, uri = y, isNew = False } )
        ( Decode.at ["track"] <| ( Decode.field "name" Decode.string ) )
        ( Decode.at ["track"] <| ( Decode.field "uri" Decode.string ) )

searchResultDecoder : Decode.Decoder (List SearchResult)
searchResultDecoder =
    let
        artistDecoder =
            Decode.at["artists", "items"]
                <| Decode.list
                <| Decode.map4 SearchResult
                ( Decode.field "name" Decode.string )
                ( Decode.field "id" Decode.string )
                ( Decode.field
                    "type"
                    ( Decode.map searchResultTypeDecoder Decode.string) )
                ( Decode.field "uri" Decode.string )
        trackDecoder =
            Decode.at["tracks", "items"]
                <| Decode.list
                <| Decode.map4 SearchResult
                ( Decode.field "name" Decode.string )
                ( Decode.field "id" Decode.string )
                ( Decode.field
                    "type"
                    (Decode.map searchResultTypeDecoder Decode.string) )
                ( Decode.field "uri" Decode.string )
    in
        trackDecoder

searchResultTypeDecoder : String -> SearchResultType
searchResultTypeDecoder val =
    case val of
        "artist" -> Artist
        _ -> Track

playlistDecoder : Decode.Decoder Playlist
playlistDecoder =
    Decode.map2 Playlist
        ( Decode.field "name" Decode.string )
        ( Decode.field "id" Decode.string )

playlistsDecoder : Decode.Decoder (List Playlist)
playlistsDecoder =
    Decode.at ["items"]
        <| Decode.list
        <| playlistDecoder

doApiRequest : String -> Body -> String -> Querystring -> String -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
doApiRequest verb body endpoint querystring token fn decoder =
    let
        absoluteUrl =
            "https://api.spotify.com/v1" ++
            endpoint ++
            "?" ++
            (Querystring.toString querystring)

        request = Http.request
            { method = verb
            , headers = [Http.header "Authorization" ("Bearer " ++ token)]
            , url = absoluteUrl
            , body = body
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False }
    in
        Http.send fn request
