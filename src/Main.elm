import Html exposing (Html, button, div, ul, li, p, pre, input, h1, h2, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Querystring
import Account
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
        , update = update }

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
                Cmd.batch
                    [ Spotify.currentUser token GetCurrentUser
                    , Spotify.fetchPlaylists token PlaylistResults ]
    in
    (
        { searchResults = { results = [], hasSearched = False, error = "" }
        , playlists = { playlists = [], error = "" }
        , selectedPlaylist = Nothing
        , identity = Anonymous
        , oAuthToken = oAuthToken
        , message = Nothing
        },
        cmd
    )

-- MODEL
type Identity
    = Anonymous
    | OAuth Spotify.User

type alias Model =
    { searchResults : Search.Model
    , playlists : Playlists.Model
    , selectedPlaylist : Maybe PlaylistEditor.Model
    , identity : Identity
    , oAuthToken : Maybe String
    , message : Maybe String
    }

model : Model
model =
    { searchResults = { results = [], hasSearched = False, error = "" }
    , playlists = { playlists = [], error = "" }
    , selectedPlaylist = Nothing
    , identity = Anonymous
    , oAuthToken = Nothing
    , message = Nothing
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
    | GetCurrentUser (Result Http.Error Spotify.User )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SavePlaylistTracks (Ok response) ->
            let
                new = case model.selectedPlaylist of
                    Nothing -> { name = "", id = "", images = [], tracks = [] }
                    Just sp ->
                        { sp | tracks = sp.tracks |> List.map Spotify.existingTrack }
            in
                ({ model | message = Just "Playlist Saved", selectedPlaylist = Just new }, Cmd.none)

        SavePlaylistTracks (Err error) ->
            ({ model | message = Just (toString error) }, Cmd.none)

        SavePlaylistResult (Ok response) ->
            let
                playlist = case model.selectedPlaylist of
                    Nothing -> Nothing
                    Just sp -> Just
                        { name = response.name
                        , id = response.id
                        , images = response.images
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
                ({ model | selectedPlaylist = playlist, message = Just "Playlist Saved" }, cmd)

        SavePlaylistResult (Err err) ->
            ({ model | message = Just (toString err) }, Cmd.none)

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
                            { name = playlist.name, id = playlist.id, images = []}
                            SavePlaylistResult
            in
                ( model, cmd )

        SelectPlaylist playlist ->
            let
                newPlaylist =
                    { name = playlist.name
                    , id = playlist.id
                    , images = playlist.images
                    , tracks = [] }

                cmd = case model.oAuthToken of
                    Nothing -> Cmd.none
                    Just token -> case (String.isEmpty newPlaylist.id) of
                        True -> Cmd.none
                        False ->
                            Spotify.getPlaylistTracks
                            token
                            playlist.id
                            PlaylistTracksResult
            in
                ({ model | selectedPlaylist = Just newPlaylist }, cmd)

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
                        , images = playlist.images
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
                searchTermPresent = not (String.isEmpty term)
                cmd = case model.oAuthToken of
                    Nothing -> Cmd.none
                    Just token -> case searchTermPresent of
                        False -> Cmd.none
                        True ->
                            Spotify.search
                            term
                            token
                            SearchResults
            in
                case searchTermPresent of
                    False -> ({ model | searchResults = { error = "", hasSearched = True, results = [] }}, cmd)
                    True -> (model, cmd)

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
            ({ model | searchResults = { results = response, hasSearched = True, error = "" }}, Cmd.none)

        SearchResults (Err error) ->
            ({ model | searchResults = { results = [], hasSearched = True, error = (toString error) }}, Cmd.none)

        GetCurrentUser (Ok response) ->
            ({ model | identity = OAuth response }, Cmd.none)

        GetCurrentUser (Err error) ->
            ({ model | message = Just (toString error) }, Cmd.none)

-- VIEW
feedbackView : Maybe String -> Html Msg
feedbackView message =
    case message of
        Nothing -> Html.text ""
        Just message ->
            div [ class "alert alert-success" ] [ p [] [ text message ] ]

view : Model -> Html Msg
view model =
    case model.identity of
        Anonymous -> div [] [ Authorise.view False, feedbackView model.message ]
        OAuth user ->
            div [ class "container-fluid fillHeight" ]
            [ div [ class "row fillHeight" ]
                [ div [ class "col-md-2 sidebar" ] [ div [ class "mt-4 mb-4 text-center" ] [ Account.view user ]
                , Playlists.view model.playlists FetchPlaylists SelectPlaylist ]
                , div [ class "col-md-8 pl-0 pr-0 main" ]
                    [ div [ class "row" ]
                        [ div [ class "col-md-12" ]
                            [ searchInputView (model.oAuthToken /= Nothing) ] ]
                            , div [ class "pl-4 pr-4" ]
                                  [ feedbackView model.message
                                  , Search.view model.searchResults SearchResultSelected ] ]
                , div [ class "col-md-2 pl-0 pr-0 sidebar" ] [ PlaylistEditor.view model.selectedPlaylist SavePlaylist ] ]
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
