module Playlists exposing (view, Model)
import Html exposing (..)
import Html.Events exposing (onClick)
import Spotify

type alias Model =
    { playlists : (List Spotify.Playlist)
    , error : String
    }

itemView : Spotify.Playlist -> (Spotify.Playlist -> msg) -> Html msg
itemView playlist selectFn =
    li []
    [ button [ onClick (selectFn playlist) ] [ text playlist.name ]
    ]

view : Model -> msg -> (Spotify.Playlist -> msg) -> Html msg
view model refreshMsg selectFn =
    let
        hasError = not (String.isEmpty model.error)
        noPlaylists = (List.isEmpty model.playlists)
    in
        case hasError of
            True ->
                div [] [ text ("ERROR: " ++ model.error) ]
            False ->
                div []
                [ button [ onClick refreshMsg ] [ text "Refresh" ]
                , ul [] (List.map (\x -> itemView x selectFn) model.playlists) ]
