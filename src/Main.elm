import Html exposing (Html, button, div, ul, li, pre, input, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Querystring
import Authorise
import Spotify

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
    { searchResults : SearchResultsModel
    , oAuthToken : Maybe String
    }

type alias SearchResultsModel =
    { results : List (Spotify.SearchResult)
    , error : String
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
            ({ model | searchResults = { results = [], error = "error!" }}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    div []
    [ searchView (model.oAuthToken /= Nothing)
    , searchResultsView model.searchResults
    --, ul [] (List.map searchResultsView model.includedTracks)
    ]

searchView : Bool -> Html Msg
searchView isAuthorised =
    div []
    [ Authorise.view isAuthorised
    , h1 [] [ text "Search" ]
    , input 
    [ placeholder "Start typing a track name or artist..."
    , onInput PerformSearch
    ] []
    ]

searchResultsListItem : Spotify.SearchResult -> Html Msg
searchResultsListItem model =
    li []
        [ pre [] [ text model.name ]
        , pre [] [ text model.id ]]

searchResultsView : SearchResultsModel -> Html Msg
searchResultsView model =
    ul [] ( List.map searchResultsListItem model.results )
