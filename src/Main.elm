import Html exposing (Html, button, div, ul, li, pre, input, h1, h2, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Querystring
import Authorise
import PlaylistEditor exposing (view, Model)
import Playlists exposing (..)
import Spotify
import Search exposing (view, Model)

main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
       }

getPlaylists : String -> Cmd Msg
getPlaylists token =
    Spotify.fetchPlaylists token PlaylistResults

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
        , playlists = { playlists = [], error = "" }
        , selectedPlaylist = Nothing
        , oAuthToken = Tuple.second
            <| Maybe.withDefault (Nothing, Nothing)
            <| List.head queryStringItems
        },
        Cmd.none
    )

-- MODEL
type alias Model =
    { searchResults : Search.Model
    , playlists : Playlists.Model
    , selectedPlaylist : Maybe PlaylistEditor.Model
    , oAuthToken : Maybe String
    }

model : Model
model =
    { searchResults = { results = [], error = "" }
    , playlists = { playlists = [], error = "" }
    , selectedPlaylist = Nothing
    , oAuthToken = Just ""
    }

-- UPDATE
type Msg
    = FetchPlaylists
    | PlaylistResults (Result Http.Error (List Spotify.Playlist))
    | SelectPlaylist Spotify.Playlist
    | PerformSearch String
    | SearchResults (Result Http.Error (List Spotify.SearchResult))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectPlaylist playlist ->
            ({ model | selectedPlaylist = Just playlist }, Cmd.none)
        FetchPlaylists ->
            (model, (getPlaylists (Maybe.withDefault "" model.oAuthToken)))
        PerformSearch term ->
            (model, (getSearch term (Maybe.withDefault "" model.oAuthToken)))
        PlaylistResults (Ok response) ->
            ({ model | playlists = { playlists = response, error = "" }}, Cmd.none)
        PlaylistResults (Err error) ->
            ({ model | playlists = { playlists = [], error = (toString error)}}, Cmd.none)
        SearchResults (Ok response) ->
            ({ model | searchResults = { results = response, error = "" }}, Cmd.none)
        SearchResults (Err error) ->
            ({ model | searchResults = { results = [], error = (toString error) }}, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    div [ class "container" ]
    [ h1 [ class "text-center" ] [ text "elm-mixtape" ]
    , div [ class "row" ]
        [ div [ class "col-md-6" ] [ PlaylistEditor.view model.selectedPlaylist ]
        , div [ class "col-md-6" ] [ searchInputView (model.oAuthToken /= Nothing)
            , Search.view model.searchResults ] ]
    , Playlists.view model.playlists FetchPlaylists SelectPlaylist
    ]

searchInputView : Bool -> Html Msg
searchInputView isAuthorised =
    div []
    [ Authorise.view isAuthorised
    , h2 [] [ text "Search" ]
    , input
    [ placeholder "Start typing a track name or artist..."
    , type_ "search"
    , class "form-control "
    , onInput PerformSearch
    ] []
    ]
