module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, style)
import Point2 exposing (..)
import Direction exposing (Direction(..))
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Time
import List.Extra as List

---- MODEL ----


type alias Model =
    { lane : Int
    , gearShiftPath : List Direction
    , gearShiftIndex : Int
    , secondsLeft : Float
    , metersLeft : Float
    , metersPerSecond : Float
    , npcCars : List Car
    , previousKeys : List Key
    , keys : List Key
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
    , previousKeys = []
    , keys = []
    }


init : ( Model, Cmd Msg )
init =
    ( newModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg
    | Step Time.Posix

addCmdNone model =
    (model, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model |> addCmdNone

        KeyMsg keyMsg ->
            { model | keys = Keyboard.update keyMsg model.keys } |> addCmdNone

        Step _ ->
            step model |> addCmdNone

secondsPerStep = 1000 / 16

step : Model -> Model
step model =
    { model
        | metersLeft = model.metersLeft - secondsPerStep * model.metersPerSecond
        , lane =
            (if isKeyPressed model (Keyboard.Character "A") then
                model.lane - 1
            else if isKeyPressed model (Keyboard.Character "D") then
                model.lane + 1
            else
                model.lane)
            |> clamp 0 (laneCount - 1)
        , gearShiftIndex =
            if (arrowPressed model |> Just) == (nextGearDirection model |> Maybe.map Direction.toPoint) then
                model.gearShiftIndex + 1
            else if (arrowPressed model |> Just) == (previousGearDirection model |> Maybe.map Direction.toPoint) then
                model.gearShiftIndex - 1
            else
                model.gearShiftIndex
        , previousKeys = model.keys
    }

nextGearDirection : Model -> Maybe Direction
nextGearDirection model =
    List.getAt model.gearShiftIndex model.gearShiftPath

previousGearDirection : Model -> Maybe Direction
previousGearDirection model =
    model.gearShiftPath
        |> List.getAt (model.gearShiftIndex - 1)
        |> Maybe.map Direction.reverse

laneCount : number
laneCount =
    3

isKeyPressed : Model -> Key -> Bool
isKeyPressed model key =
    List.any ((==) key) model.keys && (List.any ((==) key) model.previousKeys |> not)


isKeyDown : Model -> Key -> Bool
isKeyDown model key =
    List.any ((==) key) model.keys


isKeyReleased : Model -> Key -> Bool
isKeyReleased model key =
    List.any ((==) key) model.previousKeys


arrowPressed : Model -> Point2 Int
arrowPressed model =
    (if Keyboard.Arrows.arrows model.keys == Keyboard.Arrows.arrows model.previousKeys then
        Point2.zero
    else
        Keyboard.Arrows.arrows model.keys)
    |> Point2.mirrorY

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm Apps working!" ]
        , viewGearShift { x = 200, y = 200 } { x = 200, y = 200 } model
        --, model.pressedKeys |> Debug.toString |> text
        --, arrowPressed model |> debugShow
        , debugShow model.gearShiftIndex
        , nextGearDirection model |> Maybe.map Direction.toPoint |> debugShow
        , arrowPressed model |> Just |> debugShow
        ]

debugShow : a -> Html msg
debugShow =
    Debug.toString >> text

viewGearShift : Point2 Float -> Point2 Float -> Model -> Html Msg
viewGearShift position size model =
    let
        forwardPath =
            List.foldl
                (\direction (html, position1) -> (drawDirection position1 50 direction :: html, moveDirection direction 50 position1 ))
                ([], Point2.zero)
                (model.gearShiftPath |> List.drop model.gearShiftIndex)
                |> (\(html, _) -> html)

        reversePath =
            List.foldl
                (\direction (html, position1) -> (drawDirection position1 50 direction :: html, moveDirection direction 50 position1 ))
                ([], Point2.zero)
                (model.gearShiftPath
                    |> List.take model.gearShiftIndex
                    |> List.reverse
                    |> List.map Direction.reverse)
                |> (\(html, _) -> html)
    in

    div (positionAndSize position size)
        (forwardPath ++ reversePath)

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
    Direction.toPoint direction
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
        , Time.every secondsPerStep Step
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
