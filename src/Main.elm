module Main exposing (Dice, Model, Msg(..), drawDice, drawDot, drawDots, init, main, subscriptions, update, view)

import Browser
import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task



--MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--MODEL


type alias Dice =
    { dieFace : Int
    , rolls : Int
    }


type alias Model =
    { dices : Array Dice }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (fromList [Dice 1 0, Dice 1 0])
    , Cmd.none
    )



--UPDATE


type Msg
    = Start
    | Roll Int
    | Rolling Int Int
    | AddDice

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
          let
            random i = Random.generate (Rolling i) (Random.int 1 6)
          in
            (model , Cmd.batch <| List.map random (List.range 0 <| Array.length model.dices))

        Roll i ->
            ( model
            , Random.generate (Rolling i) (Random.int 1 6)
            )

        Rolling i tmpFace ->
            case get i model.dices of
                Nothing ->
                    ( model, Cmd.none )
                Just dice ->
                    if dice.rolls == 6 then
                        ( { model | dices = Array.set i (Dice tmpFace 0) model.dices } , Cmd.none )
                    else
                        ( { model | dices = Array.set i (Dice tmpFace (dice.rolls+1)) model.dices }
                        , Process.sleep 100 |> Task.perform (\_ -> Roll i)
                        )

        AddDice ->
          ( { model | dices = Array.push (Dice 1 0) model.dices } , Cmd.none )


--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (Array.toList <| Array.map drawDice model.dices)
        , button [ onClick Start ] [ Html.text "Roll" ]
        , button [ onClick AddDice ] [ Html.text "Add Dice" ]
        ]


drawDice : Dice -> Html Msg
drawDice dice =
    svg [ Svg.Attributes.width "50", Svg.Attributes.height "50" ]
        [ g []
            [ rect [ x "0", y "0", Svg.Attributes.width "50", Svg.Attributes.height "50", fill "white", stroke "black", strokeWidth "5", rx "5", ry "5" ] []
            , drawDots dice
            ]
        ]


drawDots : Dice -> Html Msg
drawDots dice =
    case dice.dieFace of
        1 ->
            drawDot "25" "25"

        2 ->
            g [] [ drawDot "13" "25", drawDot "37" "25" ]

        3 ->
            g [] [ drawDot "13" "13", drawDot "25" "25", drawDot "37" "37" ]

        4 ->
            g [] [ drawDot "13" "37", drawDot "37" "37", drawDot "13" "13", drawDot "37" "13" ]

        5 ->
            g [] [ drawDot "13" "37", drawDot "37" "37", drawDot "13" "13", drawDot "37" "13", drawDot "25" "25" ]

        6 ->
            g [] [ drawDot "13" "37", drawDot "37" "37", drawDot "13" "13", drawDot "37" "13", drawDot "13" "25", drawDot "37" "25" ]

        _ ->
            g [] []


drawDot : String -> String -> Html Msg
drawDot x y =
    circle [ cx x, cy y, r "5", fill "black" ] []
