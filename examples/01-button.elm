import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model = Int


model : Model
model =
  0



-- UPDATE


type Msg
  = Öka
  | Minska
  | Återställ


update : Msg -> Model -> Model
update msg model =
  case msg of
    Öka ->
      model + 1

    Minska ->
      model - 1

    Återställ ->
      0



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Minska ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Öka ] [ text "+" ]
    , div [] [button [ onClick Återställ ] [ text "Återställ" ]]
    ]
