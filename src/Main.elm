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
    , gearShiftPath = getGearShiftPath
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
    currentGear model |> toFloat |> (*) 10


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
            if (KeyHelper.arrowPressed model |> Just) == (nextGearDirection model |> Maybe.map Direction.toPoint) then
                model.gearShiftIndex + 1

            else if (KeyHelper.arrowPressed model |> Just) == (previousGearDirection model |> Maybe.map Direction.toPoint) then
                model.gearShiftIndex - 1

            else
                model.gearShiftIndex
        , metersPerSecond =
            model.metersPerSecond
                + 1
                |> clamp 0 (getMaxMetersPerSecond model)
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


currentGear : Model -> Int
currentGear model =
    model.gearShiftIndex // 4


moveInPath : List Direction -> Point2 Int
moveInPath path =
    List.foldl (\a b -> a |> Direction.toPoint |> Point2.add b) Point2.zero path


getGearShiftPath : List Direction
getGearShiftPath =
    getGearShiftPathHelper (Random.initialSeed 123123) 100 Set.empty []


getGearShiftPathHelper : Random.Seed -> Int -> Set ( Int, Int ) -> List Direction -> List Direction
getGearShiftPathHelper seed stepsLeft set path =
    let
        ( direction, seed1 ) =
            Random.step Direction.random seed

        position =
            moveInPath (direction :: path) |> (\a -> ( a.x, a.y ))

        direction1 =
            if Set.member position set then
                Right

            else
                direction
    in
    if stepsLeft == 0 then
        path

    else
        getGearShiftPathHelper
            seed1
            (stepsLeft - 1)
            (Set.insert position set)
            (direction1 :: path)


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ gameView model
        , debugView { x = 0, y = 730 } model
        ]


debugView : Point2 Float -> Model -> Html Msg
debugView position model =
    div (positionAndSize position { x = 500, y = 100 })
        [
        ]


gameView : Model -> Html Msg
gameView model =
    div (style "overflow" "hidden" :: positionAndSize Point2.zero screenSize)
        [ backgroundView Point2.zero screenSize model
        , imageView { x = screenSize.x / 2 - toFloat Images.playerCar.size.x / 2, y = screenSize.y - 250 } Images.playerCar
        , viewGearShift { x = 800, y = 100 } { x = 290, y = 290 } model
        ]


imageView : Point2 Float -> Image -> Html msg
imageView position image =
    img
        (src image.source :: positionAndSize position (image.size |> Point2.map toFloat))
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
            ([ style "background-color" "green" ] ++ positionAndSize { x = 0, y = screenSize.y / 2 } { x = screenSize.x, y = screenSize.y / 2 })
            []
        , div (positionAndSize position size)
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
            ([ style "background-color" "lightblue" ] ++ positionAndSize Point2.zero { x = size.x, y = size.y / 2 })
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


debugShow : a -> Html msg
debugShow =
    Debug.toString >> text


viewGearShift : Point2 Float -> Point2 Float -> Model -> Html Msg
viewGearShift position size model =
    let
        gearUpText =
            if currentGear model == 0 then
                div ([ style "font-size" "50px", style "font-family" "Consolas, Arial" ] ++ positionAndSize { x = -350, y = 100 } { x = 450, y = 30 })
                    [ text "Car idle, gear up! →" ]
            else
                div [] []

        getPath path index incrementBy =
            List.foldl
                (\direction ( html, position1, index1 ) ->
                    ( drawDirection (Point2.add position1 (Point2.map ((*) 0.5) size)) 50 direction index1 :: html
                    , moveInDirection direction 50 position1
                    , index1 + incrementBy
                    )
                )
                ( [], Point2.zero, index )
                path
                |> (\( html, _, _ ) -> html)

        forwardPath =
            getPath (model.gearShiftPath |> List.drop model.gearShiftIndex) 0 1

        reversePath =
            getPath
                (model.gearShiftPath
                    |> List.take model.gearShiftIndex
                    |> List.reverse
                    |> List.map Direction.reverse
                )
                -1
                -1
    in
    div (positionAndSize position size)
        [ gearUpText
        , div (style "overflow" "hidden" :: positionAndSize Point2.zero size)
            (reversePath ++ forwardPath)
        ]


positionAndSize : Point2 Float -> Point2 Float -> List (Html.Attribute msg)
positionAndSize position size =
    [ style "position" "absolute"
    , style "left" (px position.x)
    , style "top" (px position.y)
    , style "width" (px size.x)
    , style "height" (px size.y)
    ]


moveInDirection : Direction -> number -> Point2 number -> Point2 number
moveInDirection direction length point =
    Direction.toPoint direction
        |> Point2.map ((*) length)
        |> Point2.add point


drawDirection : Point2 Float -> Float -> Direction -> Int -> Html Msg
drawDirection position length direction stepsFromCurrentGear =
    let
        thickness =
            10.0

        length1 =
            length + thickness / 2

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
                    { x = -thickness / 2, y = 0 }
            )
                |> Point2.add position

        color =
            if stepsFromCurrentGear < 0 then
                "#AA0000FF"

            else if stepsFromCurrentGear == 0 then
                "#000000FF"

            else if stepsFromCurrentGear == 1 then
                "#666666FF"

            else if stepsFromCurrentGear == 2 then
                "#999999FF"

            else
                "#AAAAAAFF"
    in
    div ([ style "background-color" color ] ++ positionAndSize position1 size)
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
        , init = \_ -> newModel |> addCmdNone
        , update = update
        , subscriptions = subscriptions
        }
