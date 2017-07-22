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
        li [ class "selectable"
            , onClick (selectFn playlist)
            , style [ ("background", "kljfldksf") ] ]
        [ div
            []
            [ div
                []
                [ h1 [ class "h4" ] [ text playlist.name ] ]
            , button
                [ class "btn btn-primary hide", onClick (selectFn playlist) ]
                [ text "Select" ] ] ]

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
                div [ class "container-fluid" ]
                [ h1 [] [ text "Your Playlists" ]
                , ul [ class "row list-unstyled" ] (List.map (\x -> itemView x selectFn) model.playlists)
                , button [ class "btn btn-primary", onClick refreshMsg ] [ text "Refresh" ]
                , button [ class "btn btn-primary", onClick (selectFn { name = "untitled", id = "", image = Nothing }) ] [ text "Add new..." ] ]
