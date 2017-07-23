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
        mediumImage = List.head
            <| List.filter
                (\x -> (Tuple.second x) == Spotify.Medium)
                playlist.images

        img = case mediumImage of
            Nothing -> ""
            Just img -> Tuple.first img
    in
        li [ class "selectable playlist-item pt-4 pb-4"
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
                [ h1 [ class "h4 text-uppercase text-center" ] [ text "Your Playlists" ]
                , ul [ class "list-unstyled" ] (List.map (\x -> itemView x selectFn) model.playlists)
                , button [ class "btn btn-primary", onClick refreshMsg ] [ text "Refresh" ]
                , button [ class "btn btn-primary", onClick (selectFn { name = "untitled", id = "", images = []}) ] [ text "Add new..." ] ]
