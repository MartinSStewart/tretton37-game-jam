module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, style)
import Point2 exposing (..)
import Direction exposing (Direction(..))
import Keyboard exposing (Key(..))
import Keyboard.Arrows

---- MODEL ----


type alias Model =
    { lane : Int
    , gearShiftPath : List Direction
    , gearShiftIndex : Int
    , secondsLeft : Float
    , metersLeft : Float
    , metersPerSecond : Float
    , npcCars : List Car
    , pressedKeys : List Key
    }

type alias Car =
    { lane : Int
    , metersPerSecond : Float
    , metersLeft : Float
    , destroyed : Bool
    }


newCar : Int -> Float -> Float -> Car
newCar lane metersPerSecond metersLeft =
    { lane = lane
    , metersPerSecond = metersPerSecond
    , metersLeft = metersLeft
    , destroyed = False
    }


newModel : Model
newModel =
    { lane = 0
    , gearShiftPath = [ Up, Left, Down, Down, Right ]
    , gearShiftIndex = 0
    , secondsLeft = 100.0
    , metersLeft = 10000.0
    , metersPerSecond = 0.0
    , npcCars = [ newCar 0 0 9999.0 ]
    , pressedKeys = []
    }


init : ( Model, Cmd Msg )
init =
    ( newModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg

addCmdNone model =
    (model, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model |> addCmdNone

        KeyMsg keyMsg ->
            { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys } |> addCmdNone




---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm Apps working!" ]
        , viewGearShift { x = 200, y = 200 } { x = 200, y = 200 } model
        ]

viewGearShift : Point2 Float -> Point2 Float -> Model -> Html Msg
viewGearShift position size model =
    let
        path =
            List.foldl
                (\direction (html, position1) -> (drawDirection position1 50 direction :: html, moveDirection direction 50 position1 ))
                ([], Point2.zero)
                model.gearShiftPath
                |> (\(html, _) -> html)
    in

    div (positionAndSize position size)
        path

positionAndSize : Point2 Float -> Point2 Float -> List (Html.Attribute msg)
positionAndSize position size =
    [ style "position" "absolute"
    , style "left" (px position.x)
    , style "top" (px position.y)
    , style "width" (px size.x)
    , style "height" (px size.y)
    ]

moveDirection : Direction -> number -> Point2 number -> Point2 number
moveDirection direction length point =
    Direction.directionToPoint direction
        |> Point2.map ((*) length)
        |> Point2.add point

drawDirection : Point2 Float -> Float -> Direction -> Html Msg
drawDirection position length direction =
    let
        a =
            moveDirection direction length Point2.zero

        thickness = 10.0
        length1 = length + thickness / 2
        size =
            case direction of
                Left ->
                    { x = length1, y = thickness }
                Right ->
                    { x = length1, y = thickness }
                Up ->
                    { x = thickness, y = length1 }
                Down ->
                    { x = thickness, y = length1 }

        position1 =
            (case direction of
                Left ->
                    { x = -length1, y = -thickness / 2 }
                Right ->
                    { x = 0, y = -thickness / 2 }
                Up ->
                    { x = -thickness / 2, y = -length1 }
                Down ->
                    { x = -thickness / 2, y = 0 })
                |> Point2.add position
    in
    div ([ style "background-color" "black" ] ++ (positionAndSize position1 size))
        []

px : Float -> String
px value =
    String.fromFloat value ++ "px"

---- SUBSCRIPTION ----

subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
