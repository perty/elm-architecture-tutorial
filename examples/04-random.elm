import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (..)
import Random



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  {
    dieFace1 : Int
    ,dieFace2 : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1 1, Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace randomPoint)

    NewFace (newFace1, newFace2) ->
      (Model newFace1 newFace2, Cmd.none)


randomPoint : Random.Generator (Int, Int)
randomPoint =
        Random.pair (Random.int 1 6) (Random.int 1 6)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ img [ src (findUrl model.dieFace1) ] []
    , img [ src (findUrl model.dieFace2) ] []
    , button [ onClick Roll ] [ text "Roll" ]
    ]

findUrl : Int -> String
findUrl n =
    "http://www.brechtvervaeke.net/Frontpage/oefeningen/Dobbelsteen/dobbel" ++ (toString n) ++ ".gif"
