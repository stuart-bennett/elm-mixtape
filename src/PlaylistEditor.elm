module PlaylistEditor exposing (view, Model)
import Html exposing (..)

type alias Model =
    { title : String
    }

view : Maybe Model -> Html msg
view model =
    case model of
        Nothing ->
            noPlaylistSelectedView
        _ ->
            editorView (Maybe.withDefault { title = "" } model)

editorView : Model -> Html msg
editorView model =
    div []
    [ h1 [] [ text model.title ]
    ]

noPlaylistSelectedView : Html msg
noPlaylistSelectedView =
    div []
    [ span [] [ text "Nothing selected!" ]
    ]
