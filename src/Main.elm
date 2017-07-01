import Html exposing (Html, button, div, ul, li, pre, input, h1, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Querystring
import Authorise
import Playlists exposing (view)
import Spotify
import Search exposing (view, Model)

main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
       }

getSearch : String -> String -> Cmd Msg
getSearch searchTerm token =
    Spotify.search searchTerm token SearchResults

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
        { searchResults = { results = [], error = "" }
        , oAuthToken = Tuple.second
            <| Maybe.withDefault (Nothing, Nothing)
            <| List.head queryStringItems
        },
        Cmd.none
    )

-- MODEL
type alias Model =
    { searchResults : Search.Model
    , oAuthToken : Maybe String
    }


model : Model
model =
    { searchResults = { results = [], error = "" }
    , oAuthToken = Just ""
    }

-- UPDATE
type Msg
    = Increment
    | PerformSearch String
    | SearchResults (Result Http.Error (List Spotify.SearchResult))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            (model, Cmd.none)
        PerformSearch term ->
            (model, (getSearch term (Maybe.withDefault "" model.oAuthToken)))
        SearchResults (Ok response) ->
            ({ model | searchResults = { results = response, error = "" }}, Cmd.none)
        SearchResults (Err error) ->
            ({ model | searchResults = { results = [], error = (toString error) }}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    div []
    [ searchInputView (model.oAuthToken /= Nothing)
    , Search.view model.searchResults
    , Playlists.view
    ]

searchInputView : Bool -> Html Msg
searchInputView isAuthorised =
    div []
    [ Authorise.view isAuthorised
    , h1 [] [ text "Search" ]
    , input
    [ placeholder "Start typing a track name or artist..."
    , onInput PerformSearch
    ] []
    ]
