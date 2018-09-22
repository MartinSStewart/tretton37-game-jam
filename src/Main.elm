port module Main exposing (..)

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
import Maybe.Extra as Maybe
import Ease
import Ports
import Json


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
    , randomSeed : Random.Seed
    , pause : Bool
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


addRandomCar : Model -> Model
addRandomCar model =
    model.randomSeed
        |> Random.step
            (Random.float 0 1
                |> Random.andThen
                    (\a ->
                        if a < min 0.05 (0.01 * model.metersPerSecond / (gearToMetersPerSecond GearShift.maxGear)) then
                            Random.map
                                (\lane -> (newCar lane (model.metersPerSecond * 0.1) (model.metersLeft - roadMetersVisible)) |> Just)
                                (Random.int 0 (laneCount - 1))
                        else
                            Random.constant Nothing
                    )
            )
        |> (\(car, seed) ->
            { model
                | npcCars = Maybe.toList car ++ model.npcCars
                , randomSeed = seed
            })


newModel : Int -> Model
newModel seed =
    let
        randomSeed =
            Random.initialSeed seed
    in

    { targetLane = 1
    , currentLane = 1
    , gearShiftPath = GearShift.getGearShiftPath randomSeed
    , gearShiftIndex = 0
    , secondsLeft = 100.0
    , metersLeft = 10000.0
    , metersPerSecond = 0.0
    , npcCars = [ newCar 0 0 9500.0 ]
    , previousKeys = []
    , keys = []
    , randomSeed = randomSeed
    , pause = False
    }



---- UPDATE ----


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg
    | Step Time.Posix


addCmdNone model =
    ( model, Cmd.none )


gearToMetersPerSecond gear =
    gear + 1 |> toFloat |> logBase 1.2 |> (*) 5


metersPerSecondToKph =
    3.6


debugMode : Bool
debugMode =
    True

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model |> addCmdNone

        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.keys
            in

            { model
                | keys = newKeys
                , pause =
                    if debugMode && KeyHelper.isPressed { keys = newKeys, previousKeys = model.keys } (Keyboard.Character "p") then
                        not model.pause
                    else
                        model.pause
            }
            |> addCmdNone

        Step _ ->
            step model


framesPerSecond =
    60


secondsPerStep =
    1000 / framesPerSecond


step : Model -> (Model, Cmd Msg)
step model =
    { model
        | metersLeft =
            model.metersLeft - model.metersPerSecond / secondsPerStep
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
        , npcCars =
            model.npcCars
                |> List.filter
                    (\npcCar -> npcCar.metersLeft < model.metersLeft + 300 && npcCar.metersLeft > model.metersLeft - roadMetersVisible - 50)
                |> List.map
                    (\npcCar -> { npcCar | metersLeft = npcCar.metersLeft - npcCar.metersPerSecond / secondsPerStep })

        , metersPerSecond =
            model.metersPerSecond
                + 1
                |> clamp 0 (model |> GearShift.currentGear |> gearToMetersPerSecond)
        , previousKeys = model.keys
        , secondsLeft = model.secondsLeft - 0.016
    }
    |> addRandomCar
    |> handleCollision

handleCollision : Model -> (Model, Cmd Msg)
handleCollision model =
    let
        npcCarTuples =
            List.map
                (\npcCar ->
                    if (not npcCar.destroyed)
                        && (abs (toFloat npcCar.lane - model.currentLane) |> (\a -> a < 0.8))
                        && (abs (npcCar.metersLeft - model.metersLeft + 385 {-hacky offset-}) < 30) then
                        ({ npcCar
                            | destroyed = True
                            , metersPerSecond = npcCar.metersPerSecond / 2
                         }
                        , True
                        )
                    else
                        (npcCar, False)
                )
                model.npcCars

        hasCollided =
            List.any (\(_, collided) -> collided) npcCarTuples
    in

    ({ model
        | npcCars = npcCarTuples |> List.map (\(npcCar, _) -> npcCar)
        , gearShiftIndex =
            if hasCollided then
                model.gearShiftIndex // 2 |> max GearShift.shiftsPerGear
            else
                model.gearShiftIndex
        , metersPerSecond =
            if hasCollided then
                model.metersPerSecond / 2
            else
                model.metersPerSecond
    }
    , if hasCollided then
        playSound "collision.ogg"
    else
        Cmd.none
    )


playSound : String -> Cmd msg
playSound soundName =
    Ports.PlaySound { soundName = soundName, loop = False }
        |> Json.encodePortOutMsg
        |> Ports.portOut


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
        [ model.npcCars |> Helper.debugShow
        ]


gameView : Model -> Html Msg
gameView model =
    let
        npcCars =
            model.npcCars
                |> List.sortBy (\npcCar -> npcCar.metersLeft)
                |> List.map
                    (\npcCar ->
                        let
                            distanceT =
                                (roadMetersVisible + npcCar.metersLeft - model.metersLeft) / roadMetersVisible |> (\a -> a * 1 + 0.8 |> Ease.inExpo |> (+) -0.3)

                            imageSizeRatio =
                                roadFarWidth / roadNearWidth

                            imageSize =
                                Images.npcCar.size |> Point2.map (toFloat >> (*) ((1 - imageSizeRatio) * distanceT + imageSizeRatio))
                        in

                        img ([ src Images.npcCar.source ]
                                ++ Helper.positionAndSize
                                    (getRoadPos
                                        model
                                        ((toFloat npcCar.lane + 0.5) / laneCount)
                                        distanceT
                                        |> Point2.add (Point2.map ((*) -0.5) imageSize))
                                    imageSize
                            )
                            []
                    )
                |> div []
    in

    div (style "overflow" "hidden" :: Helper.positionAndSize Point2.zero screenSize)
        [ backgroundView Point2.zero screenSize model
        , npcCars
        , imageView
            { x = screenSize.x / 2 - toFloat Images.playerCar.size.x / 2
            , y = screenSize.y - 150
            }
            Images.playerCar
        , GearShift.view { x = 800, y = 100 } { x = 290, y = 290 } model
        , speedometerView { x = 200, y = 100 } model
        ]


imageView : Point2 Float -> Image -> Html msg
imageView position image =
    img
        (src image.source :: Helper.positionAndSize position (image.size |> Point2.map toFloat))
        []


roadFarWidth =
    200


roadNearWidth =
    1000


roadMetersVisible =
    500


getRoadPos : Model -> Float -> Float -> Point2 Float
getRoadPos model tx ty =
    let
        offset =
            (model.currentLane + 0.5) / laneCount
    in

    { x = ((roadNearWidth - roadFarWidth) * ty + roadFarWidth) * (tx - offset) + screenSize.x / 2
    , y = (screenSize.y - screenSize.y / 2) * ty + screenSize.y / 2
    }


speedometerView : Point2 Float -> Model -> Html msg
speedometerView position model =
    div (Helper.positionAndSize position { x = 200, y = 200 })
        [ model.metersPerSecond * metersPerSecondToKph
            |> String.fromFloat
            |> (\a -> a ++ "KPH")
            |> text
        ]


backgroundView : Point2 Float -> Point2 Float -> Model -> Html Msg
backgroundView position size model =
    let
        drawLine t isDashed =
            Svg.polyline
                ((if isDashed then
                    [ Svg.Attributes.strokeDasharray "100"
                    , Svg.Attributes.strokeDashoffset (model.metersLeft |> (*) 10 |> String.fromFloat)
                    ]

                  else
                    []
                 )
                    ++ [ Svg.Attributes.stroke "white"
                       , Svg.Attributes.strokeWidth "5"
                       , svgPoints
                            [ getRoadPos model t 0
                            , getRoadPos model t 1
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
                        [ getRoadPos model 0 0
                        , getRoadPos model 1 0
                        , getRoadPos model 1 1
                        , getRoadPos model 0 1
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
        , if model.pause then
              Sub.none
          else
              Time.every secondsPerStep Step
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> newModel 123125 |> addCmdNone
        , update = update
        , subscriptions = subscriptions
        }
