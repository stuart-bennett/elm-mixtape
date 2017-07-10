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
    = SavePlaylist PlaylistEditor.Model
    | SavePlaylistResult (Result Http.Error Spotify.Playlist)
    | FetchPlaylists
    | SavePlaylistTracks (Result Http.Error String)
    | PlaylistTracksResult (Result Http.Error Spotify.Tracklist)
    | PlaylistResults (Result Http.Error (List Spotify.Playlist))
    | SelectPlaylist Spotify.Playlist
    | PerformSearch String
    | SearchResultSelected Spotify.SearchResult
    | SearchResults (Result Http.Error (List Spotify.SearchResult))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SavePlaylistTracks (Ok response) ->
            (model, Cmd.none)

        SavePlaylistTracks (Err error) ->
            (model, Cmd.none)

        SavePlaylistResult (Ok response) ->
            let
                playlist = case model.selectedPlaylist of
                    Nothing -> Nothing
                    Just sp -> Just
                        { name = response.name
                        , id = response.id
                        , tracks = sp.tracks }

                cmd = case model.selectedPlaylist of
                    Nothing -> Cmd.none
                    Just sp -> Spotify.savePlaylistTracks
                        ( Maybe.withDefault "" model.oAuthToken )
                        sp.id
                        ( List.map Tuple.second sp.tracks )
                        SavePlaylistTracks
            in
                ({ model | selectedPlaylist = playlist }, Cmd.none)

        SavePlaylistResult (Err _) -> (model, Cmd.none)

        SavePlaylist playlist ->
            let
                cmd = Spotify.savePlaylist (
                    Maybe.withDefault "" model.oAuthToken )
                    { name = playlist.name, id = playlist.id }
                    SavePlaylistResult
            in
                ( model, cmd )

        SelectPlaylist playlist ->
            let
                newPlaylist = Just
                    { name = playlist.name
                    , id = playlist.id
                    , tracks = [] }
                cmd = Spotify.getPlaylistTracks
                    ( Maybe.withDefault "" model.oAuthToken )
                    playlist.id
                    PlaylistTracksResult
            in
                ({ model | selectedPlaylist = newPlaylist }, cmd)

        SearchResultSelected searchResult ->
            let
                old = Maybe.withDefault
                    { name = ""
                    , id = ""
                    , tracks = [] }
                    model.selectedPlaylist

                newValue =
                    { old | tracks = ( ( searchResult.name, searchResult.uri ) :: old.tracks ) }
            in
                case model.selectedPlaylist of
                    Nothing ->
                        (model, Cmd.none)
                    Just playlist ->
                        ({ model | selectedPlaylist = Just newValue }, Cmd.none)

        PlaylistTracksResult (Ok response) ->
            case model.selectedPlaylist of
                Nothing -> (model, Cmd.none)
                Just playlist ->
                    ({ model
                    | selectedPlaylist = Just
                        { name = playlist.name
                        , id = playlist.id
                        , tracks = ( List.append playlist.tracks response ) } },
                    Cmd.none)

        PlaylistTracksResult (Err error) ->
            ({ model | playlists =
                { playlists = []
                , error = (toString error) } },
            Cmd.none)

        FetchPlaylists -> 
            (model, (getPlaylists (Maybe.withDefault "" model.oAuthToken)))

        PerformSearch term ->
            let
                cmd = getSearch term ( Maybe.withDefault "" model.oAuthToken)
            in
                (model, cmd)

        PlaylistResults (Ok response) ->
            let
                playlists = { playlists = response, error = "" }
            in
                ({ model | playlists = playlists }, Cmd.none)

        PlaylistResults (Err error) ->
            let
                playlists = { playlists = [], error = (toString error)}
            in
                ({ model | playlists = playlists }, Cmd.none)

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
        [ div [ class "col-md-6" ] [ PlaylistEditor.view model.selectedPlaylist SavePlaylist ]
        , div [ class "col-md-6" ] [ searchInputView (model.oAuthToken /= Nothing)
            , Search.view model.searchResults SearchResultSelected ] ]
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
    , class "form-control"
    , onInput PerformSearch
    ] []
    ]
