module PlaylistEditor exposing (view, Model)
import Html exposing (..)
import Html.Attributes exposing (class)
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
    [ h2 [] [ text model.name ]
    , ul [ class "list-group" ] (List.map tracksView model.tracks)
    ]

tracksView : String -> Html msg
tracksView track =
    li [ class "list-group-item" ] [ text track ]

noPlaylistSelectedView : Html msg
noPlaylistSelectedView =
    div []
    [ h2 [ class "h2"] [ text "Nothing selected!" ]
    , p [] [ text "Select a playlist to begin editing" ]
    ]
