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
    1000



-- MODEL


type alias Model =
    { now : Time
    , gameState : GameState
    , direction : Direction
    , snake : Snake
    , apple : Coord
    }


type GameState
    = RUN
    | END


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
    , tail =
        [ { x = 54
          , y = 50
          }
        ]
    }


initialApple : Coord
initialApple =
    { x = 10
    , y = 30
    }


init : ( Model, Cmd Msg )
init =
    ( { now = 0, gameState = RUN, direction = WEST, snake = initialSnake, apple = initialApple }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | Presses Command


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            updateGame model newTime

        Presses command ->
            updateDirection model command


updateGame : Model -> Time -> ( Model, Cmd Msg )
updateGame model newTime =
    ( case model.gameState of
        RUN ->
            { model | now = newTime, snake = updateSnake model, gameState = updateGameState model }

        END ->
            { model | now = newTime }
    , Cmd.none
    )


updateSnake : Model -> Snake
updateSnake model =
    let
        snake =
            model.snake
    in
    case model.direction of
        NORTH ->
            moveSnake 0 -4 snake

        SOUTH ->
            moveSnake 0 4 snake

        WEST ->
            moveSnake -4 0 snake

        EAST ->
            moveSnake 4 0 snake


moveSnake : Int -> Int -> Snake -> Snake
moveSnake updateX updateY snake =
    { snake | head = { y = snake.head.y + updateY, x = snake.head.x + updateX }, tail = moveTail snake.head snake.tail }


moveTail : Coord -> List Coord -> List Coord
moveTail head tail =
    head :: List.take (List.length tail - 1) tail


updateGameState : Model -> GameState
updateGameState model =
    if withinBounds model && notHittingSelf model then
        RUN
    else
        END


withinBounds : Model -> Bool
withinBounds model =
    let
        head =
            model.snake.head
    in
    (head.y > 4) && (head.y < 96) && (head.x > 4) && (head.x < 96)


notHittingSelf : Model -> Bool
notHittingSelf model =
    True


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
            ]
        ]
        [ svg [ viewBox "0 0 100 100", width "500px" ]
            (gameView model)
        , div []
            [ text (toString (Date.fromTime model.now))
            , text " Game state: "
            , text (toString model.gameState)
            ]
        ]


gameView : Model -> List (Svg Msg)
gameView model =
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
    [ rect [ width "100", height "100", fill "lightBlue" ]
        []
    , line
        [ x1 "50", y1 "50", x2 "100", y2 "50", stroke "black", transform rotate ]
        []
    ]
        ++ snakeView model.snake
        ++ appleView model.apple


snakeView : Snake -> List (Svg Msg)
snakeView snake =
    headView snake ++ tailView snake


headView : Snake -> List (Svg Msg)
headView snake =
    [ circle [ cx (toString snake.head.x), cy (toString snake.head.y), r "2" ] []
    ]


tailView : Snake -> List (Svg Msg)
tailView snake =
    List.map tailPart snake.tail


tailPart : Coord -> Svg Msg
tailPart coord =
    circle [ cx (toString coord.x), cy (toString coord.y), r "2", fill "green" ] []


appleView : Coord -> List (Svg Msg)
appleView coord =
    [ circle [ cx (toString coord.x), cy (toString coord.y), r "2", fill "red" ] []
    ]
