module Main exposing (..)

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
    let
        clock =
            clockFn
    in
    div []
        [ svg [ viewBox "0 0 100 100", width "300px" ]
            clock
        , div []
            [ text (toString (Date.fromTime model))
            ]
        ]


clockFn : List (Svg Msg)
clockFn =
    [ g [ fill "green", stroke "black", transform "translate(50,50), rotate(0)" ]
        [ circle [ cx "0", cy "0", r "50", fill "white" ] []
        , polygon [ points "0,0 5,5 25,0 5, -5", transform "translate(15,0)" ] []
        , line [ x1 "0", y1 "0", x2 "15", y2 "0" ] []
        ]
    ]
