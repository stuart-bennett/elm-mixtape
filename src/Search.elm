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
                ul [ class "list-unstyled" ]
                    ( List.map (\x -> (listItemView x selectFn)) model.results)

listItemView : Spotify.SearchResult -> (Spotify.SearchResult -> msg) -> Html msg
listItemView model selectFn =
    let
        smallImage = model.images
            |> List.filter (\x -> (Tuple.second x) == Spotify.Medium)
            |> List.head
        image = case smallImage of
            Nothing -> ""
            Just tuple -> Tuple.first tuple
    in
        li [ class "col-md-4 selectable searchResult"
            , onClick (selectFn model) ]
            [ img [ class "searchResult-image", src image ] []
            , div [ class "" ]
                [ h1 [ class "h5" ] [ text model.name ]
                , span [] [ text ("type: " ++ (toString model.type_)) ]
                , span [] [ text model.id ] ] ]
