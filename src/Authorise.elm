module Authorise exposing (..)
import Html exposing (Html, div, text, a)
import Html.Attributes exposing (href)

view : Bool -> Html msg
view isAuthorised =
    let
        authoriseUrl = "https://accounts.spotify.com/authorize" ++
            "/?client_id=97528257022e49baa4718572bbe536a5" ++
            "&redirect_uri=http://localhost:8000/index.html" ++
            "&scope=playlist-modify-public" ++
            "&response_type=token"
    in
        case isAuthorised of
            True ->
                Html.text ""
            False ->
                div []
                    [
                        a [ href authoriseUrl ]
                          [ text "Authorise" ]
                    ]
