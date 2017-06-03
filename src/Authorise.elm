module Authorise exposing (..)
import Html exposing (Html, div, text, a)
import Html.Attributes exposing (href)

view : Html msg
view =
    let
        authoriseUrl = "https://accounts.spotify.com/authorize" ++
            "/?client_id=97528257022e49baa4718572bbe536a5" ++
            "&redirect_uri=http://localhost:8000/index.html" ++
            "&response_type=token"
    in
    div []
        [
            a [ href authoriseUrl ]
              [ text "here in authorize" ]
        ]
