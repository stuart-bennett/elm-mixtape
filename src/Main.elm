import Html exposing (Html, button, div, ul, li, input, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode

main =
    Html.program
        { init = init 
        , subscriptions = subscriptions
        , view = view
        , update = update
        }

srDecoder : Decode.Decoder Int
srDecoder =
    Decode.at["artists", "total"] Decode.int

getSearch : String -> Cmd Msg
getSearch searchTerm =
    let
        url = "http://localhost:8000/sample.json"
        request = Http.get url srDecoder
    in
        Http.send SearchResults request


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- INIT
init : (Model, Cmd Msg)
init =
    (
        { includedTracks = [ "You haven't selected any tracks yet, use the search bar to find new tracks." ]
        },
        Cmd.none
    )

-- MODEL
type alias Model = 
    { includedTracks : List String
    }

model : Model
model =
    { includedTracks = [ "You haven't selected any tracks yet, use the search bar to find new tracks." ]
    }


-- UPDATE
type Msg
    = Increment
    | PerformSearch String
    | SearchResults (Result Http.Error Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            (model, Cmd.none)
        PerformSearch term ->
            (model, getSearch term)
        SearchResults (Ok response) ->
            ({ model | includedTracks = [(toString response)] }, Cmd.none)
        SearchResults (Err error) ->
            ({ model | includedTracks = [ "error!", (toString error) ] }, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    div []
    [ searchView ""
    , ul [] (List.map songsView model.includedTracks)
    ]

searchView : string -> Html Msg
searchView model =
    div []
    [ h1 [] [ text "Search" ]
    , input 
    [ placeholder "Start typing a track name or artist..."
    , onInput PerformSearch
    ] []
    ]

songsView : String -> Html Msg
songsView model =
    li [] [ text model ]
