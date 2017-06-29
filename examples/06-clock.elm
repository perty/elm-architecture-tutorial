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
    angleSeconds = angleCalculator (Date.second theDateNow)
    angleMinutes = angleCalculator (Date.minute theDateNow)
    angleHours = angleCalculator (analogHour theDateNow)
    clock = clockFn angleSeconds angleMinutes angleHours
  in
    div [] [
        svg [ viewBox "0 0 100 100", width "300px" ]
        clock
        , div [] [
            text (toString (Date.fromTime model))
        ]
      ]

angleCalculator: Int -> Float
angleCalculator time =
    ((toFloat time) - 15.0) * 2.0 * pi / 60.0

analogHour:  Date.Date -> Int
analogHour theDateNow =
    (((Date.hour theDateNow) %  12) * 5 +  (Date.minute theDateNow) // 12 )

hand: Float -> Float -> String -> Svg Msg
hand angle length color =
    let
        xAngle = toString (50 + length * cos angle)
        yAngle = toString (50 + length * sin angle)
    in
        line [ x1 "50", y1 "50", x2 xAngle, y2 yAngle, stroke color ] []

clockFn : Float -> Float -> Float -> List (Svg Msg)
clockFn angleSeconds angleMinutes angleHours =
    [
       circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
    ]
    ++ tickMarks
    ++ [
         hand angleSeconds 40 "#023963"
        ,hand angleMinutes 40 "#1df14f"
        ,hand angleHours 30 "#f1361d"
        ]

tickMarks: List (Svg Msg)
tickMarks  =
    List.map tick (List.range 1 60)

tick : Int -> (Svg Msg)
tick minute =
    let
        tickStart = if minute % 5 == 0 then 35 else 42
        angle = angleCalculator minute
        xEnd = toString (50 + 45 * cos angle)
        yEnd = toString (50 + 45 * sin angle)
        xStart = toString (50 + tickStart * cos angle)
        yStart = toString (50 + tickStart * sin angle)
    in
        line [ x1 xStart, y1 yStart, x2 xEnd, y2 yEnd, stroke "#000000"] []
