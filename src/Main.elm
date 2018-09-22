module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, style)
import Point2 exposing (..)


---- MODEL ----


type alias Model =
    { carLane : Int
    , gearShiftPath : List Direction
    , gearShiftIndex : Int
    }


type Direction = Left | Right | Up | Down

directionToPoint2 : Direction -> Point2 Int
directionToPoint2 direction =
    case direction of
        Left ->
            Point2 -1 0

        Right ->
            Point2 1 0

        Up ->
            Point2 0 -1

        Down ->
            Point2 0 1



init : ( Model, Cmd Msg )
init =
    (   { carLane = 0
        , gearShiftPath = [ Up, Left ]
        , gearShiftIndex = 0
        }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm Appis working!" ]
        , viewGearShift Point2.zero { x = 0, y = 0 } model
        ]

viewGearShift : Point2 Float -> Point2 Float -> Model -> Html Msg
viewGearShift position size model =

    div [ style "background-color" "black"
        , style "position" "absolute"
        , style "left" (String.fromFloat position.x ++ "px")
        , style "top" (String.fromFloat position.y ++ "px")
        , style "width" "5px"
        , style "height" "50px" ]
        []

drawDirection : Point2 Float -> Float -> Direction -> Html Msg
drawDirection position length direction =
    let
        size =
    in

    div [ style "background-color" "black"
        , style "position" "absolute"
        , style "left" (String.fromFloat position.x ++ "px")
        , style "top" (String.fromFloat position.y ++ "px")
        , style "width" "5px"
        , style "height" "50px" ]
        []

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
