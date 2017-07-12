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


speed =
    100



-- MODEL


type alias Model =
    { now : Time
    , direction : Direction
    , snake : Snake
    }


type Direction
    = NORTH
    | SOUTH
    | WEST
    | EAST


type alias Coord =
    { x : Int
    , y : Int
    }


type alias Snake =
    { head : Coord
    , tail : List Coord
    }


type Command
    = LEFT
    | UP
    | RIGHT
    | DOWN
    | NONE


initialSnake : Snake
initialSnake =
    { head =
        { x = 50
        , y = 50
        }
    , tail = []
    }


init : ( Model, Cmd Msg )
init =
    ( { now = 0, direction = WEST, snake = initialSnake }, Cmd.none )



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
    ( { model | now = newTime, snake = updateSnake model newTime }, Cmd.none )


updateSnake : Model -> Time -> Snake
updateSnake model newTime =
    let
        snake =
            model.snake
    in
    case model.direction of
        NORTH ->
            conditionUpdate (snake.head.y > 1) 0 -1 snake

        SOUTH ->
            conditionUpdate (snake.head.y < 100) 0 1 snake

        WEST ->
            conditionUpdate (snake.head.x > 1) -1 0 snake

        EAST ->
            conditionUpdate (snake.head.x < 100) 1 0 snake


conditionUpdate : Bool -> Int -> Int -> Snake -> Snake
conditionUpdate condition updateX updateY snake =
    if condition then
        { snake | head = { y = snake.head.y + updateY, x = snake.head.x + updateX } }
    else
        snake


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
        [ Time.every speed Tick
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
        ++ snakeView model.snake


snakeView : Snake -> List (Svg Msg)
snakeView snake =
    [ circle [ cx (toString snake.head.x), cy (toString snake.head.y), r "5" ] []
    ]
