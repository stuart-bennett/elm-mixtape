module Spotify exposing (
    search,
    currentUser,
    getPlaylistTracks,
    savePlaylist,
    fetchPlaylists,
    savePlaylistTracks,
    trackFromSearchResult,
    existingTrack,
    User,
    SearchResult,
    Playlist,
    PlaylistTrack,
    Image,
    ImageSize(..))

import Json.Decode as Decode
import Json.Encode as Encode
import Http exposing (..)
import String exposing (concat)
import Querystring exposing (..)

type SearchResultType = Artist
    | Track

type alias User =
    { id : String
    , linkToProfile : String
    , images : List Image }

type alias PlaylistTrack =
    { title : String
    , artists : List String
    , album : String
    , uri : String
    , images: List Image
    , isNew : Bool }

trackFromSearchResult : SearchResult -> PlaylistTrack
trackFromSearchResult x =
    { title = x.name
    , uri = x.uri
    , images = x.images
    , artists = x.artists
    , album = x.album
    , isNew = True }

existingTrack : PlaylistTrack -> PlaylistTrack
existingTrack x =
    { title = x.title
    , uri = x.uri
    , images = x.images
    , artists = x.artists
    , album = x.album
    , isNew = False }

type alias SearchResult =
    { name : String
    , id : String
    , type_ : SearchResultType
    , uri : String
    , images : List Image
    , artists : List String
    , album : String
    }

type alias Playlist =
    { name: String
    , id: String
    , images: List Image
    }

type ImageSize
    = Large
    | Medium
    | Small

type alias Image = (String, ImageSize)

currentUser : String -> (Result Http.Error User -> msg) -> Cmd msg
currentUser token fn =
    let
        endpoint = "/me"
    in
        doApiRequest
            "GET"
            Http.emptyBody
            endpoint
            Querystring.empty
            token
            fn
            userDecoder

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
            , ("type", "track")
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
userDecoder : Decode.Decoder User
userDecoder =
    Decode.map3 User
    ( Decode.field "id" Decode.string )
    ( Decode.field "href" Decode.string )
    ( Decode.at [ "images" ] <| imagesDecoder )


tracklistDecoder : Decode.Decoder (List PlaylistTrack)
tracklistDecoder =
    Decode.at["items"]
        <| Decode.list
        <| Decode.map5
            ( \title artists album uri images -> { title = title, uri = uri, artists = artists, album = album, images = images, isNew = False } )
            ( Decode.at ["track"] <| ( Decode.field "name" Decode.string ) )
            ( Decode.at ["track", "artists"] <| Decode.list ( Decode.field "name" Decode.string ) )
            ( Decode.at ["track", "album"] <| ( Decode.field "name" Decode.string ) )
            ( Decode.at ["track"] <| ( Decode.field "uri" Decode.string ) )
            ( Decode.at ["track", "album", "images"] <| imagesDecoder )

searchResultDecoder : Decode.Decoder (List SearchResult)
searchResultDecoder =
    let
        trackDecoder =
            Decode.at["tracks", "items"]
                <| Decode.list
                <| Decode.map7 SearchResult
                ( Decode.field "name" Decode.string )
                ( Decode.field "id" Decode.string )
                ( Decode.field "type"
                    ( Decode.map searchResultFromString Decode.string ) )
                ( Decode.field "uri" Decode.string )
                ( Decode.at ["album", "images"] <| imagesDecoder )
                ( Decode.at ["artists"] <| Decode.list ( Decode.field "name" Decode.string ) )
                ( Decode.at ["album"] <| ( Decode.field "name" Decode.string ) )
    in
        trackDecoder

imagesDecoder : Decode.Decoder (List Image)
imagesDecoder =
    Decode.list imageDecoder

imageDecoder : Decode.Decoder Image
imageDecoder =
    Decode.map2 (\url width -> (url, width))
    ( Decode.field "url" Decode.string )
    ( Decode.field "width"
        ( Decode.map imageSizeFromInt Decode.int ))

imageSizeFromInt : Int -> ImageSize
imageSizeFromInt val =
    case val of
        640 -> Large
        300 -> Medium
        _ -> Small

searchResultFromString : String -> SearchResultType
searchResultFromString val =
    case val of
        "artist" -> Artist
        _ -> Track

playlistDecoder : Decode.Decoder Playlist
playlistDecoder =
    Decode.map3 Playlist
        ( Decode.field "name" Decode.string )
        ( Decode.field "id" Decode.string )
        ( Decode.at ["images"] <| imagesDecoder )

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
