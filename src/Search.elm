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
                ul [ class "list-unstyled row" ]
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
        li [ class "col-md-4  mb-4"] [ div [ class "card selectable searchResult"
            , onClick (selectFn model) ]
            [ img [ class "card-img-top searchResult-image", src image ] []
            , div [ class "card-block" ]
                [ h1 [ class "h6" ] [ text model.name ]
                , ul [ class "list-unstyled" ] ( List.map (\x -> li [] [ text x ])  model.artists )
                , span [] [ text model.album ] ] ] ]
