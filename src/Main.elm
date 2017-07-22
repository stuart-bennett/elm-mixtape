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
        oAuthToken = Tuple.second
            <| Maybe.withDefault (Nothing, Nothing)
            <| List.head queryStringItems

        cmd = case oAuthToken of
            Nothing-> Cmd.none
            Just token ->
                Spotify.fetchPlaylists
                token
                PlaylistResults
    in
    (
        { searchResults = { results = [], error = "" }
        , playlists = { playlists = [], error = "" }
        , selectedPlaylist = Nothing
        , oAuthToken = oAuthToken
        },
        cmd
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
    | PlaylistTracksResult (Result Http.Error (List Spotify.PlaylistTrack))
    | PlaylistResults (Result Http.Error (List Spotify.Playlist))
    | SelectPlaylist Spotify.Playlist
    | PerformSearch String
    | SearchResultSelected Spotify.SearchResult
    | SearchResults (Result Http.Error (List Spotify.SearchResult))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SavePlaylistTracks (Ok response) ->
            let
                new = case model.selectedPlaylist of
                    Nothing -> { name = "", id = "", tracks = [] }
                    Just sp ->
                        { sp | tracks = sp.tracks |> List.map Spotify.existingTrack }
            in
                ({ model | selectedPlaylist = Just new }, Cmd.none)

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
                    Just sp -> case model.oAuthToken of
                        Nothing -> Cmd.none
                        Just token ->
                            Spotify.savePlaylistTracks
                            token
                            response.id
                            ( sp.tracks |> List.filter (\x -> x.isNew) |> List.map (\x -> x.uri) )
                            SavePlaylistTracks
            in
                ({ model | selectedPlaylist = playlist }, cmd)

        SavePlaylistResult (Err _) -> (model, Cmd.none)

        SavePlaylist playlist ->
            let
                cmd = case (String.isEmpty playlist.id) of
                    False -> case model.selectedPlaylist of
                        Nothing -> Cmd.none
                        Just sp ->
                            Spotify.savePlaylistTracks
                            ( Maybe.withDefault "" model.oAuthToken )
                            sp.id
                            ( sp.tracks |> List.filter (\x -> x.isNew) |> List.map (\x -> x.uri) )
                            SavePlaylistTracks

                    True -> case model.oAuthToken of
                        Nothing -> Cmd.none
                        Just token ->
                            Spotify.savePlaylist
                            token
                            { name = playlist.name, id = playlist.id, image = Nothing }
                            SavePlaylistResult
            in
                ( model, cmd )

        SelectPlaylist playlist ->
            let
                newPlaylist = Just
                    { name = playlist.name
                    , id = playlist.id
                    , tracks = [] }

                cmd = case model.oAuthToken of
                    Nothing -> Cmd.none
                    Just token ->
                        Spotify.getPlaylistTracks
                        token
                        playlist.id
                        PlaylistTracksResult
            in
                ({ model | selectedPlaylist = newPlaylist }, cmd)

        SearchResultSelected searchResult ->
            let
                newValue = case model.selectedPlaylist of
                    Nothing -> Nothing
                    Just sp ->
                        Just { sp
                        | tracks = ( Spotify.trackFromSearchResult searchResult ) :: sp.tracks }
            in
                case newValue of
                    Nothing ->
                        (model, Cmd.none)
                    Just playlist ->
                        ({ model | selectedPlaylist = Just playlist }, Cmd.none)

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
            (model, Spotify.fetchPlaylists (Maybe.withDefault "" model.oAuthToken) PlaylistResults)

        PerformSearch term ->
            let
                cmd = case model.oAuthToken of
                    Nothing -> Cmd.none
                    Just token ->
                        Spotify.search
                        term
                        token
                        SearchResults
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
        div [ class "container-fluid fillHeight" ]
        [ div [ class "row fillHeight" ]
            [ div [ class "col-md-2 sidebar" ] [ Playlists.view model.playlists FetchPlaylists SelectPlaylist ]
            , div [ class "col-md-8 pl-0 pr-0 main" ]
                [ div [ class "row" ] 
                    [ div [ class "col-md-12" ]
                        [ searchInputView (model.oAuthToken /= Nothing) ] ]
                        , div [ class "pl-4 pr-4" ] [ Search.view model.searchResults SearchResultSelected ] ]
            , div [ class "col-md-2 sidebar" ] [ PlaylistEditor.view model.selectedPlaylist SavePlaylist ] ]
        ]

searchInputView : Bool -> Html Msg
searchInputView isAuthorised =
    div [ class "mb-4 pt-5 pb-5 pt-4 pb-4 searchInput" ]
    [ Authorise.view isAuthorised
    , div [ class "pl-4 pr-4" ] [ input
    [ placeholder "Start typing a track name or artist..."
    , type_ "search"
    , class "form-control"
    , onInput PerformSearch
    ] [] ]
    ]
