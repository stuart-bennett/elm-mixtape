import Html exposing (Html, button, div, ul, li, input, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Querystring exposing (..)

main =
    Html.programWithFlags
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

type alias Flags =
    { locationFragment: String
    }

-- INIT
init : Flags -> (Model, Cmd Msg)
init flags =
    let
        -- Tokens appear in the fragment during oAuth "implicit" flow
        queryStringItems = Querystring.getItems flags.locationFragment
    in
    (
        { includedTracks = [ "You haven't selected any tracks yet, use the search bar to find new tracks." ]
        , oAuthToken = Tuple.second
            <| Maybe.withDefault (Nothing, Nothing)
            <| List.head queryStringItems
        },
        Cmd.none
    )

-- MODEL
type alias Model = 
    { includedTracks : List String
    , oAuthToken : Maybe String
    }

model : Model
model =
    { includedTracks = [ "You haven't selected any tracks yet, use the search bar to find new tracks." ]
    , oAuthToken = Just ""
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
