module Playlists exposing (view, Model)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Spotify

type alias Model =
    { playlists : (List Spotify.Playlist)
    , error : String
    }

itemView : Spotify.Playlist -> (Spotify.Playlist -> msg) -> Html msg
itemView playlist selectFn =
    let
        img = case playlist.image of
            Nothing -> ""
            Just img -> img
    in
        li [ class "selectable pt-2 pb-2"
            , onClick (selectFn playlist) ]
            [ div
                []
                [ span [] [ text playlist.name ] ]
            , button
                [ class "btn btn-primary hide", onClick (selectFn playlist) ]
                [ text "Select" ] ]

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
                div [ class "" ]
                [ h1 [ class "h4" ] [ text "Your Playlists" ]
                , ul [ class "list-unstyled" ] (List.map (\x -> itemView x selectFn) model.playlists)
                , button [ class "btn btn-primary", onClick refreshMsg ] [ text "Refresh" ]
                , button [ class "btn btn-primary", onClick (selectFn { name = "untitled", id = "", image = Nothing }) ] [ text "Add new..." ] ]
