module Account exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Spotify

view : Spotify.User -> Html msg
view user =
    let
        image = List.head user.images
        imageSrc = case image of
            Nothing ->  "http://fillmurray.com/100/100"
            Just img -> Tuple.first img
    in
        div []
            [ img [ class "rounded-circle", src imageSrc ] []
            , div [ ] [ text user.id ] ]

