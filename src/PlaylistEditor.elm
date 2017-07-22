module PlaylistEditor exposing (view, Model)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, contenteditable, src)
import Spotify

type alias Model =
    { name : String
    , id : String
    , tracks : List Spotify.PlaylistTrack }

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
                , ul [ class "list-unstyled" ] ( List.map tracksView model.tracks ) ]
            False ->
                div []
                [ h2 [ contenteditable True ] [ text model.name ]
                , p [] [ text "No tracks yet!" ] ]

tracksView : Spotify.PlaylistTrack -> Html msg
tracksView track =
    let
        smallImage = track.images
            |> List.filter (\x -> (Tuple.second x) == Spotify.Small)
            |> List.head
        image = case smallImage of
            Nothing -> ""
            Just tuple -> Tuple.first tuple
    in
        li
        [ class "mt-2 mb-2" ]
        [ div [ class "media" ]
            [ img [ class "d-flex mr-3", src image ] []
            , div [ class "media-body" ]
                [ span [] [ text track.title ] ] ] ]

noPlaylistSelectedView : Html msg
noPlaylistSelectedView =
    div []
    [ h2 [ class "h2"] [ text "Nothing selected!" ]
    , p [] [ text "Select a playlist to begin editing" ]
    ]
