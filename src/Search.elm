module Search exposing (view, Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Spotify

type alias Model =
    { results : List (Spotify.SearchResult)
    , error : String
    }

-- VIEW
view : Model -> (Spotify.SearchResult -> msg) -> Html msg
view model selectFn =
    let
        hasError = not (String.isEmpty model.error)
    in
        case hasError of
            True ->
                span [] [ text model.error ]
            False ->
                ul [ class "list-group" ] 
                    ( List.map (\x -> (listItemView x selectFn)) model.results)

listItemView : Spotify.SearchResult -> (Spotify.SearchResult -> msg) -> Html msg
listItemView model selectFn =
    li [  class "list-group-item selectable"
        , onClick (selectFn model) ]
        [ h1 [ class "h4" ] [ text model.name ]
        , span [] [ text ("type: " ++ (toString model.type_)) ]
        , span [] [ text model.id ]]
