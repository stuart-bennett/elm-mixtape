module Search exposing (view, Model)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Spotify

type alias Model =
    { results : List (Spotify.SearchResult)
    , hasSearched: Bool
    , error : String
    }

-- VIEW
view : Model -> (Spotify.SearchResult -> msg) -> Html msg
view model selectFn =
    let
        hasError = (not (String.isEmpty model.error), model.hasSearched && List.isEmpty model.results)
    in
        case hasError of
            (True, _) ->
                span [] [ text model.error ]
            (_, True) ->
                span [] [ text "no results!" ]
            (False, False) ->
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
        li [ class "col-md-3 mb-4"] [ div [ class "card selectable searchResult"
            , onClick (selectFn model) ]
            [ img [ class "card-img-top searchResult-image", src image ] []
            , div [ class "card-block" ]
                [ h1 [ class "h6 mb-0 text-uppercase" ] [ text model.name ]
                , ul [ class "list-unstyled" ] ( List.map (\x -> li [ class "small" ] [ text x ])  model.artists )
                , p [ class "small mt-1" ] [ text model.album ] ] ] ]
