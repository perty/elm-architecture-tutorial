module Main exposing (..)

import Char exposing (..)
import Date
import Debug
import Html exposing (Html, div)
import Html.Attributes
import Keyboard exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { now : Time
    , direction : Direction
    }


type Command
    = LEFT
    | UP
    | RIGHT
    | DOWN
    | NONE


type Direction
    = NORTH
    | SOUTH
    | WEST
    | EAST


init : ( Model, Cmd Msg )
init =
    ( { now = 0, direction = WEST }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | Presses Command


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            updateGameState model newTime

        Presses command ->
            updateDirection model command


updateGameState : Model -> Time -> ( Model, Cmd Msg )
updateGameState model newTime =
    ( { model | now = newTime }, Cmd.none )


updateDirection : Model -> Command -> ( Model, Cmd Msg )
updateDirection model command =
    case command of
        LEFT ->
            newDirection model WEST

        UP ->
            newDirection model NORTH

        RIGHT ->
            newDirection model EAST

        DOWN ->
            newDirection model SOUTH

        NONE ->
            ( model, Cmd.none )


newDirection : Model -> Direction -> ( Model, Cmd Msg )
newDirection model newDirection =
    ( { model | direction = newDirection }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5000 * millisecond) Tick
        , Keyboard.presses (\code -> Presses (codeToCommand code))
        ]


codeToCommand : Int -> Command
codeToCommand code =
    case Debug.log "Code " code of
        97 ->
            LEFT

        119 ->
            UP

        100 ->
            RIGHT

        115 ->
            DOWN

        _ ->
            NONE



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "padding", "10px" )
            , ( "border", "solid 1px" )
            , ( "max-width", "300px" )
            ]
        ]
        [ svg [ viewBox "0 0 100 100", width "300px" ]
            (game model)
        , div []
            [ text (toString (Date.fromTime model.now))
            ]
        ]


game : Model -> List (Svg Msg)
game model =
    let
        rotation =
            case model.direction of
                NORTH ->
                    270

                SOUTH ->
                    90

                WEST ->
                    180

                EAST ->
                    0

        rotate =
            "rotate(" ++ toString rotation ++ ", 50, 50)"
    in
    [ rect [ width "100", height "100", fill "#0B79CE" ]
        []
    , line
        [ x1 "50", y1 "50", x2 "100", y2 "50", stroke "black", transform rotate ]
        []
    ]
