module Playlists exposing (view, Model)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Spotify

type alias Model =
    { playlists : (List Spotify.Playlist)
    , error : String
    }

itemView : Spotify.Playlist -> (Spotify.Playlist -> msg) -> Html msg
itemView playlist selectFn =
    li [ class "list-group-item selectable", onClick (selectFn playlist) ]
    [ h1 [ class "h4" ] [ text playlist.name ]
    , button [ class "btn btn-primary hide"
             , onClick (selectFn playlist) ] [ text "Select" ] ]

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
                [ button [ class "btn btn-primary", onClick refreshMsg ] [ text "Refresh" ]
                , ul [ class "list-group" ] (List.map (\x -> itemView x selectFn) model.playlists)
                , button [ class "btn btn-primary", onClick (selectFn { name = "untitled", id = "" }) ] [ text "Add new..." ] ]
