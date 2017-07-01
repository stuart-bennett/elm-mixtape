module Playlists exposing (view, Model)
import Html exposing (..)
import Spotify

type alias Model =
    { playlists : (List Spotify.Playlist)
    , error : String
    }

view : Model -> Html msg
view model =
    let
        hasError = String.isEmpty model.error
        noPlaylists = (List.isEmpty model.playlists)
    in
        case hasError of
            True ->
                div [] [ text model.error ]
            False ->
                div [] [ text "SHOW PLAYLISTS HERE" ]

