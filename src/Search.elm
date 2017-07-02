module Search exposing (view, Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Spotify

type alias Model =
    { results : List (Spotify.SearchResult)
    , error : String
    }

-- VIEW
view : Model -> Html msg
view model =
    let
        hasError = not (String.isEmpty model.error)
    in
        case hasError of
            True ->
                span [] [ text model.error ]
            False ->
                ul [ class "list-group" ] ( List.map listItemView model.results )

listItemView : Spotify.SearchResult -> Html msg
listItemView model =
    li [ class "list-group-item selectable" ]
        [ h1 [ class "h4" ] [ text model.name ]
        , span [] [ text model.id ]]
