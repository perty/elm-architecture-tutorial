module Main exposing (..)

import Char exposing (..)
import Date
import Debug
import Html exposing (Html, div)
import Html.Attributes
import Keyboard exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Time, millisecond)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


speed =
    300



-- MODEL


type alias Model =
    { now : Time
    , score : Int
    , gameState : GameState
    , snake : Snake
    , apple : Coord
    }


type GameState
    = RUN
    | EAT
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
    { direction : Direction
    , head : Coord
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
    { direction = WEST
    , head =
        { x = 48
        , y = 48
        }
    , tail =
        [ { x = 52
          , y = 48
          }
        ]
    }


initialApple : Coord
initialApple =
    { x = 12
    , y = 36
    }


init : ( Model, Cmd Msg )
init =
    ( { now = 0, score = 0, gameState = RUN, snake = initialSnake, apple = initialApple }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | Presses Command
    | EatApple
    | NewApple ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            updateGame model newTime

        Presses command ->
            updateDirection model command

        EatApple ->
            eatApple model

        NewApple ( x, y ) ->
            newApple model (Coord (x * 4) (y * 4))


updateGame : Model -> Time -> ( Model, Cmd Msg )
updateGame model newTime =
    ( case model.gameState of
        RUN ->
            { model | now = newTime, snake = updateSnake model, gameState = updateGameState model }

        EAT ->
            model

        END ->
            { model | now = newTime }
    , nextCmd model
    )


nextCmd : Model -> Cmd Msg
nextCmd model =
    if model.apple == model.snake.head then
        send EatApple
    else
        Cmd.none


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


updateSnake : Model -> Snake
updateSnake model =
    case model.snake.direction of
        NORTH ->
            moveSnake 0 -4 model.snake model.score

        SOUTH ->
            moveSnake 0 4 model.snake model.score

        WEST ->
            moveSnake -4 0 model.snake model.score

        EAST ->
            moveSnake 4 0 model.snake model.score


moveSnake : Int -> Int -> Snake -> Int -> Snake
moveSnake updateX updateY snake score =
    { snake | head = moveHead snake.head updateX updateY, tail = moveTail snake.head snake.tail score }


moveHead : Coord -> Int -> Int -> Coord
moveHead head updateX updateY =
    { y = head.y + updateY, x = head.x + updateX }


moveTail : Coord -> List Coord -> Int -> List Coord
moveTail head tail tailLength =
    head :: List.take tailLength tail


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
    (head.y >= 4) && (head.y <= 96) && (head.x >= 4) && (head.x <= 96)


notHittingSelf : Model -> Bool
notHittingSelf model =
    True


eatApple : Model -> ( Model, Cmd Msg )
eatApple model =
    ( { model | apple = { x = -10, y = -10 }, gameState = EAT, score = model.score + 1 }
    , Random.generate NewApple randomPoint
    )


newApple : Model -> Coord -> ( Model, Cmd Msg )
newApple model coord =
    if (model.snake.head == coord) || List.member coord model.snake.tail then
        ( model, Random.generate NewApple randomPoint )
    else
        ( { model | apple = { x = coord.x, y = coord.y }, gameState = RUN }, Cmd.none )


randomPoint : Random.Generator ( Int, Int )
randomPoint =
    Random.pair (Random.int 1 24) (Random.int 1 24)


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
    ( { model | snake = newSnakeDirection model.snake newDirection }, Cmd.none )


newSnakeDirection : Snake -> Direction -> Snake
newSnakeDirection snake newDirection =
    { snake | direction = newDirection }



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
            , text " Score: "
            , text (toString model.score)
            ]
        ]


gameView : Model -> List (Svg Msg)
gameView model =
    background
        ++ directionArrow model
        ++ appleView model.apple
        ++ snakeView model.snake


background : List (Svg Msg)
background =
    [ rect [ width "100", height "100", fill "lightBlue" ]
        []
    ]


directionArrow : Model -> List (Svg Msg)
directionArrow model =
    let
        rotation =
            case model.snake.direction of
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
    [ g [ stroke "black", transform rotate ]
        [ line
            [ x1 "50", y1 "50", x2 "60", y2 "50" ]
            []
        , line
            [ x1 "50", y1 "50", x2 "40", y2 "45" ]
            []
        , line
            [ x1 "50", y1 "50", x2 "40", y2 "55" ]
            []
        ]
    ]


snakeView : Snake -> List (Svg Msg)
snakeView snake =
    tailView snake ++ headView snake


headView : Snake -> List (Svg Msg)
headView snake =
    let
        size =
            12

        y =
            snake.head.y - size // 2

        x =
            snake.head.x - size // 2

        translate =
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"
    in
    [ image [ xlinkHref (imageBasedOnDirection snake.direction), width (toString size), transform translate ] []
    ]


imageBasedOnDirection : Direction -> String
imageBasedOnDirection direction =
    case direction of
        NORTH ->
            "image/hampus-north.png"

        SOUTH ->
            "image/hampus-south.png"

        WEST ->
            "image/hampus-west.png"

        EAST ->
            "image/hampus-east.png"


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
