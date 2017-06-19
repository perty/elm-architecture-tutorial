import Date
import Html exposing (Html, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    theDateNow = (Date.fromTime model)
    angleSeconds = angleCalculator (Date.second theDateNow)  15.0 60
    angleMinutes = angleCalculator (Date.minute theDateNow) 15.0 60
    angleHours = angleCalculator ((Date.hour theDateNow) %  12) 3.0 12
  in
    div [] [
        svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , hand angleSeconds 40 "#023963"
        , hand angleMinutes 40 "#1df14f"
        , hand angleHours 30 "#f1361d"
        ]
        , div [] [
            text (toString (Date.fromTime model))
        ]
      ]

angleCalculator: Int -> Float -> Float -> Float
angleCalculator time offset ticksPerLap =
    ((toFloat time) - offset) * 2.0 * pi / ticksPerLap

hand: Float -> Float -> String -> Svg Msg
hand angle length color =
    let
        xAngle = toString (50 + length * cos angle)
        yAngle = toString (50 + length * sin angle)
    in
        line [ x1 "50", y1 "50", x2 xAngle, y2 yAngle, stroke color ] []