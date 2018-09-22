module Main exposing (..)

import Browser
import Direction exposing (Direction(..))
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, style)
import Images exposing (Image)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import List.Extra as List
import Point2 exposing (..)
import Random
import Set exposing (Set)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width, x, y)
import Time
import KeyHelper
import GearShift
import Helper


---- MODEL ----


type alias Model =
    { targetLane : Int
    , currentLane : Float
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
    { targetLane = 1
    , currentLane = 1
    , gearShiftPath = GearShift.getGearShiftPath
    , gearShiftIndex = 0
    , secondsLeft = 100.0
    , metersLeft = 10000.0
    , metersPerSecond = 0.0
    , npcCars = [ newCar 0 0 9999.0 ]
    , previousKeys = []
    , keys = []
    }



---- UPDATE ----


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg
    | Step Time.Posix


addCmdNone model =
    ( model, Cmd.none )


getMaxMetersPerSecond model =
    GearShift.currentGear model |> toFloat |> (*) 10


metersPerSecondToKph =
    3.6


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model |> addCmdNone

        KeyMsg keyMsg ->
            { model | keys = Keyboard.update keyMsg model.keys } |> addCmdNone

        Step _ ->
            step model |> addCmdNone


secondsPerStep =
    1000 / 60


step : Model -> Model
step model =
    { model
        | metersLeft = model.metersLeft - secondsPerStep * model.metersPerSecond
        , targetLane =
            (if model.metersPerSecond <= 1 then
                model.targetLane
            else if KeyHelper.isPressed model (Keyboard.Character "a") || KeyHelper.isDown model (Keyboard.Character "A") then
                model.targetLane - 1

             else if KeyHelper.isPressed model (Keyboard.Character "d") || KeyHelper.isPressed model (Keyboard.Character "D") then
                model.targetLane + 1

             else
                model.targetLane
            )
                |> clamp 0 (laneCount - 1)
        , currentLane =
            let
                targetLane1 =
                    toFloat model.targetLane
            in

            if targetLane1 < model.currentLane then
                max targetLane1 (model.currentLane - min 0.3 (0.01 * model.metersPerSecond))
            else
                min targetLane1 (model.currentLane + min 0.3 (0.01 * model.metersPerSecond))
        , gearShiftIndex =
            if (KeyHelper.arrowPressed model |> Just) == (GearShift.nextGearDirection model |> Maybe.map Direction.toPoint) then
                model.gearShiftIndex + 1

            else if (KeyHelper.arrowPressed model |> Just) == (GearShift.previousGearDirection model |> Maybe.map Direction.toPoint) then
                model.gearShiftIndex - 1

            else
                model.gearShiftIndex
        , metersPerSecond =
            model.metersPerSecond
                + 1
                |> clamp 0 (getMaxMetersPerSecond model)
        , previousKeys = model.keys
    }


laneCount : number
laneCount =
    3


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ gameView model
        , debugView { x = 0, y = 730 } model
        ]


debugView : Point2 Float -> Model -> Html Msg
debugView position model =
    div (Helper.positionAndSize position { x = 500, y = 100 })
        [
        ]


gameView : Model -> Html Msg
gameView model =
    div (style "overflow" "hidden" :: Helper.positionAndSize Point2.zero screenSize)
        [ backgroundView Point2.zero screenSize model
        , imageView { x = screenSize.x / 2 - toFloat Images.playerCar.size.x / 2, y = screenSize.y - 250 } Images.playerCar
        , GearShift.view { x = 800, y = 100 } { x = 290, y = 290 } model
        ]


imageView : Point2 Float -> Image -> Html msg
imageView position image =
    img
        (src image.source :: Helper.positionAndSize position (image.size |> Point2.map toFloat))
        []


backgroundView : Point2 Float -> Point2 Float -> Model -> Html Msg
backgroundView position size model =
    let
        roadFarWidth =
            400

        roadNearWidth =
            1000

        offset =
            (model.currentLane + 0.5) / laneCount

        getX tx ty =
            ((roadNearWidth - roadFarWidth) * ty + roadFarWidth) * tx

        drawLine t isDashed =
            Svg.polyline
                ((if isDashed then
                    [ Svg.Attributes.strokeDasharray "100"
                    , Svg.Attributes.strokeDashoffset (model.metersLeft |> (*) 0.01 |> String.fromFloat)
                    ]

                  else
                    []
                 )
                    ++ [ Svg.Attributes.stroke "white"
                       , Svg.Attributes.strokeWidth "5"
                       , svgPoints
                            [ { x = size.x / 2 + getX (t - offset) 0, y = size.y / 2 }
                            , { x = size.x / 2 + getX (t - offset) 1, y = size.y }
                            ]
                            |> Svg.Attributes.points
                       ]
                )
                []
    in
    div []
        [ div
            ([ style "background-color" "green" ] ++ Helper.positionAndSize { x = 0, y = screenSize.y / 2 } { x = screenSize.x, y = screenSize.y / 2 })
            []
        , div (Helper.positionAndSize position size)
            [ svg
                (svgPositionAndSize Point2.zero screenSize)
                [ Svg.polygon
                    [ Svg.Attributes.fill "#333333FF"
                    , svgPoints
                        [ { x = size.x / 2 + getX -offset 0, y = size.y / 2 }
                        , { x = size.x / 2 + getX (1 - offset) 0, y = size.y / 2 }
                        , { x = size.x / 2 + getX (1 - offset) 1, y = size.y }
                        , { x = size.x / 2 + getX -offset 1, y = size.y }
                        ]
                        |> Svg.Attributes.points
                    ]
                    []
                , drawLine 0 False
                , drawLine (1 / 3) True
                , drawLine (2 / 3) True
                , drawLine 1 False
                ]
            ]
        , div
            ([ style "background-color" "lightblue" ] ++ Helper.positionAndSize Point2.zero { x = size.x, y = size.y / 2 })
            []
        ]


svgPositionAndSize position size =
    [ String.fromInt position.x |> x
    , String.fromInt position.x |> y
    , String.fromInt size.x |> width
    , String.fromInt size.y |> height
    , [ position.y, position.y, size.x, size.y ]
        |> List.map String.fromInt
        |> String.join " "
        |> viewBox
    ]


svgPoints : List (Point2 Float) -> String
svgPoints points =
    points
        |> List.map (\a -> String.fromFloat a.x ++ "," ++ String.fromFloat a.y)
        |> String.join " "


screenSize =
    Point2 1280 720


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
        , init = \_ -> newModel |> addCmdNone
        , update = update
        , subscriptions = subscriptions
        }
