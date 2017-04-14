module Box exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MODEL
type alias Model = Int
model : Model
model = 0


-- UPDATE

type Msg
    = Action1
    | Action2
    | None

update : Msg -> Model -> Model
update msg model =
    case msg of
        Action1 ->
            model + 2
        Action2 ->
            model + 10
        None ->
            model


-- VIEW
view : Html Msg
view =
    div [] 
        [ button [ onClick Action1 ] [ text "#1" ]
        , div [] [ text "fdsf" ]
        , button [ onClick Action2 ] [ text "#2" ]
        ]
