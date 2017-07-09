module PlaylistEditor exposing (view, Model)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, contenteditable)
import Spotify

type alias Model =
    { name : String
    , id : String
    , tracks : (List String) }

view : Maybe Model -> (Model -> msg) -> Html msg
view model selectFn =
    case model of
        Nothing ->
            noPlaylistSelectedView
        Just playlist ->
            editorView playlist selectFn

editorView : Model -> (Model -> msg) -> Html msg
editorView model saveFn =
    let
        hasTracks = not (List.isEmpty model.tracks)
    in
        case hasTracks of
            True ->
                div []
                [ h2 [ contenteditable True ] [ text model.name ]
                , button [ class "btn btn-primary", onClick ( saveFn model ) ] [ text "Save" ]
                , ul [ class "list-group" ] ( List.map tracksView model.tracks ) ]
            False ->
                div []
                [ h2 [ contenteditable True ] [ text model.name ]
                , p [] [ text "No tracks yet!" ] ]

tracksView : String -> Html msg
tracksView track =
    li [ class "list-group-item" ] [ text track ]

noPlaylistSelectedView : Html msg
noPlaylistSelectedView =
    div []
    [ h2 [ class "h2"] [ text "Nothing selected!" ]
    , p [] [ text "Select a playlist to begin editing" ]
    ]
