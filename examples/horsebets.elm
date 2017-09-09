module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (field)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { suggestion : Suggestion
    , message : String
    , flavour : String
    }


type alias Suggestion =
    { legs : List Leg
    }


type alias Leg =
    { selections : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( { suggestion = { legs = [] }, message = "Please select something", flavour = "chans" }, Cmd.none )



-- UPDATE


type Msg
    = NewSuggestion (Result Http.Error Suggestion)
    | GenerateSuggestion
    | SetFlavour String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NewSuggestion (Ok suggestion) ->
            ( { model | suggestion = suggestion }, Cmd.none )

        NewSuggestion (Err _) ->
            ( { model | message = "Error!" }, Cmd.none )

        GenerateSuggestion ->
            ( model, generateSuggestion model )

        SetFlavour flavour ->
            ( { model | flavour = flavour }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Make suggestion" ]
        , select [ onInput SetFlavour ]
            [ option []
                [ text "chans"
                ]
            , option []
                [ text "favorit"
                ]
            ]
        , button [ onClick GenerateSuggestion ] [ text "Get suggestion" ]
        , hr [] []
        , p []
            [ text model.message
            ]
        , div []
            [ suggestionTable model
            ]
        ]


suggestionTable : Model -> Html Msg
suggestionTable model =
    table []
        [ tableHead
        , tableBody model
        ]


tableHead : Html Msg
tableHead =
    thead []
        [ tr []
            [ th [] [ text "Leg" ]
            , th [] [ text "Suggestion" ]
            ]
        ]


tableBody : Model -> Html Msg
tableBody model =
    tbody []
        (List.map2 legRow (List.range 1 8) (List.map mapLegToHtml model.suggestion.legs))


legRow : Int -> Html Msg -> Html Msg
legRow legNo suggestion =
    tr []
        [ td [] [ text (toString legNo) ]
        , td [] [ suggestion ]
        ]


mapLegToHtml : Leg -> Html Msg
mapLegToHtml leg =
    text (String.join ", " (List.map toString leg.selections))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


generateSuggestion : Model -> Cmd Msg
generateSuggestion model =
    let
        url =
            "http://localhost:8080/suggestions"

        request =
            Http.post url Http.emptyBody decodeSuggestion
    in
    Http.send NewSuggestion request


decodeSuggestion : Json.Decode.Decoder Suggestion
decodeSuggestion =
    Json.Decode.map Suggestion
        (field "legs" (Json.Decode.list decodeLeg))


decodeLeg : Json.Decode.Decoder Leg
decodeLeg =
    Json.Decode.map Leg
        (field "selections" (Json.Decode.list Json.Decode.int))
