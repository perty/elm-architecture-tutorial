module Main exposing (..)

import Date
import Html exposing (Html, div)
import Html.Attributes exposing (style)
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


type alias Model =
    Time


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( newTime, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "padding", "10px" )
            , ( "border", "solid 1px" )
            , ( "max-width", "100px" )
            ]
        ]
        [ svg [ viewBox "0 0 100 100", width "100px" ]
            (clockFn (Date.fromTime model))
        ]


clockFn : Date.Date -> List (Svg Msg)
clockFn theDateNow =
    [ g [ fill "black", stroke "black", transform "translate(50,50)" ]
        (clockFace
            ++ hourHand theDateNow
            ++ minutesHand theDateNow
            ++ secondsHand theDateNow
        )
    ]


clockFace : List (Svg Msg)
clockFace =
    [ circle [ cx "0", cy "0", r "49", fill "white" ] []
    ]
        ++ tickMarks


tickMarks : List (Svg Msg)
tickMarks =
    List.map tick (List.range 1 60)


tick : Int -> Svg Msg
tick minute =
    let
        tickStart =
            if minute % 5 == 0 then
                "35"
            else
                "42"

        angle =
            toString (minuteToAngle minute)
    in
    line [ x1 tickStart, y1 "0", x2 "49", y2 "0", transform ("rotate(" ++ angle ++ ")") ] []


minuteToAngle : Int -> Float
minuteToAngle minute =
    (toFloat minute - 15.0) * 360 / 60.0


hourHand : Date.Date -> List (Svg Msg)
hourHand theDateNow =
    let
        angle =
            minuteToAngle (hourMinute theDateNow)
    in
    [ g [ transform ("rotate(" ++ toString angle ++ ")") ]
        [ polygon [ points "0,0 4,4 20,0 4, -4", transform "translate(10,0)" ] []
        , xLine "0" "10"
        ]
    ]



-- Which minute should the hour hand point to?


hourMinute : Date.Date -> Int
hourMinute theDateNow =
    (Date.hour theDateNow % 12) * 5 + Date.minute theDateNow // 12


minutesHand : Date.Date -> List (Svg Msg)
minutesHand theDateNow =
    let
        angle =
            minuteToAngle (Date.minute theDateNow)
    in
    [ g [ transform ("rotate(" ++ toString angle ++ ")") ]
        [ polygon [ points "0,0 2,2 30,0 2, -2", transform "translate(5,0)" ] []
        , xLine "0" "5"
        ]
    ]


secondsHand : Date.Date -> List (Svg Msg)
secondsHand theDateNow =
    let
        angle =
            minuteToAngle (Date.second theDateNow)
    in
    [ g [ transform ("rotate(" ++ toString angle ++ ")") ]
        [ xLine "-5" "40"
        , circle [ cx "0", cy "0", r "5", fillOpacity "0", transform "translate(30,0)" ] []
        ]
    ]


xLine : String -> String -> Svg Msg
xLine start end =
    line [ x1 start, y1 "0", x2 end, y2 "0" ] []
