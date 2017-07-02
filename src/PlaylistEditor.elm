module PlaylistEditor exposing (view, Model)
import Html exposing (..)
import Spotify

type alias Model = Spotify.Playlist

view : Maybe Model -> Html msg
view model =
    case model of
        Nothing ->
            noPlaylistSelectedView
        Just playlist ->
            editorView playlist

editorView : Model -> Html msg
editorView model =
    div []
    [ h1 [] [ text model.name ]
    ]

noPlaylistSelectedView : Html msg
noPlaylistSelectedView =
    div []
    [ span [] [ text "Nothing selected!" ]
    ]
