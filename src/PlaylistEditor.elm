module PlaylistEditor exposing (view, Model)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, contenteditable, src)
import Spotify

type alias Model =
    { name : String
    , id : String
    , images : List (Spotify.Image)
    , tracks : List Spotify.PlaylistTrack }

view : Maybe Model -> (Model -> msg) -> Html msg
view model selectFn =
    case model of
        Nothing ->
            div [ class "pl-4 pr-4" ] [ noPlaylistSelectedView ]
        Just playlist ->
            div [ class "" ] [ editorView playlist selectFn ]

editorView : Model -> (Model -> msg) -> Html msg
editorView model saveFn =
    let
        hasTracks = not (List.isEmpty model.tracks)
        largeImage = List.head <|
            List.filter (\x -> Tuple.second x == Spotify.Large)
            model.images

        image = case largeImage of
            Nothing -> ""
            Just img -> Tuple.first img
    in
        case hasTracks of
            True ->
                div []
                [ img [ class "mw-100 mb-4", src image ] []
                , div [ class "pl-4 pr-4" ]
                    [ h2 [ contenteditable True, class "mb-4" ] [ text model.name ]
                    , button [ class "btn btn-primary", onClick ( saveFn model ) ] [ text "Save" ]
                    , ul [ class "list-unstyled" ] ( List.map tracksView model.tracks ) ] ]
            False ->
                div []
                [ img [ src image ] []
                , h2 [ contenteditable True ] [ text model.name ]
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

        primaryArtist = case (List.head track.artists) of
            Nothing -> "[unknown]"
            Just artist -> artist
    in
        li
        [ class "mt-2 mb-2" ]
        [ div [ class "media" ]
            [ img [ class "d-flex mr-3", src image ] []
            , div [ class "media-body" ]
                [ h1 [ class "h6" ] [ text track.title ]
                , span [ class "" ] [ text primaryArtist ] ] ] ]

noPlaylistSelectedView : Html msg
noPlaylistSelectedView =
    div []
    [ p [ class "actionRequired h4 mt-5 text-center" ] [ text "Select a playlist to begin editing" ] ]
