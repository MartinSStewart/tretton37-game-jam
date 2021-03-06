port module Main exposing (Car, Model, Msg(..), addCmdNone, addRandomCar, backgroundView, debugMode, debugView, decodeHighscoreRow, decodeHighscores, encodeHighscoreRow, encodeHighscores, framesPerSecond, gameView, gearToMetersPerSecond, getHighscores, getRoadPos, handleCollision, highscoreTable, laneCount, metersPerSecondToKph, newCar, newModel, nextCheckpoint, playSound, roadFarWidth, roadMetersVisible, roadNearWidth, screenSize, secondsPerStep, setHighscores, speedometerView, startingSecondsLeft, step, stopSound, subscriptions, svgPoints, svgPositionAndSize, update, view)

import Browser
import Direction exposing (Direction(..))
import Ease
import GearShift
import Helper
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Images exposing (Image)
import Json
import Json.Decode as Decode
import Json.Encode as Encode
import KeyHelper
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import List.Extra as List
import Maybe.Extra as Maybe
import Point2 exposing (..)
import Ports
import Random
import Round
import Set exposing (Set)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width, x, y)
import Time



---- MODEL ----


type alias Model =
    { targetLane : Int
    , currentLane : Float
    , gearShiftPath : List Direction
    , currentGearShiftIndex : Float
    , targetGearShiftIndex : Int
    , secondsLeft : Float
    , metersLeft : Float
    , metersPerSecond : Float
    , npcCars : List Car
    , previousKeys : List Key
    , keys : List Key
    , randomSeed : Random.Seed
    , pause : Bool
    , started : Bool
    , lastCarAddedTime : Float
    , time : Float
    , checkpointReachedTime : Float
    , highscores : Maybe (List ( String, Float ))
    , name : String
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
                        if a < (0.05 * model.metersPerSecond / gearToMetersPerSecond GearShift.maxGear) then
                            Random.map
                                (\lane -> newCar lane (model.metersPerSecond * 0.1) (model.metersLeft - roadMetersVisible) |> Just)
                                (Random.int 0 (laneCount - 1))

                        else
                            Random.constant Nothing
                    )
            )
        |> (\( car, seed ) ->
                { model
                    | npcCars = Maybe.toList car ++ model.npcCars
                    , randomSeed = seed
                    , lastCarAddedTime =
                        if car == Nothing then
                            model.lastCarAddedTime

                        else
                            model.time
                }
           )


newModel : Int -> Maybe (List ( String, Float )) -> String -> Model
newModel seed highscores name =
    let
        randomSeed =
            Random.initialSeed seed
    in
    { targetLane = 1
    , currentLane = 1
    , gearShiftPath = GearShift.getGearShiftPath randomSeed
    , targetGearShiftIndex = 0
    , currentGearShiftIndex = 0
    , secondsLeft = startingSecondsLeft
    , metersLeft = 0.0
    , metersPerSecond = 0.0
    , npcCars = []
    , previousKeys = []
    , keys = []
    , randomSeed = randomSeed
    , pause = False
    , started = False
    , lastCarAddedTime = 0
    , time = 0
    , checkpointReachedTime = -1000
    , highscores = highscores
    , name = name
    }



---- UPDATE ----


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg
    | Step Time.Posix
    | NewGame
    | GetHighscores (Result Http.Error (List ( String, Float )))
    | SetHighscores (Result Http.Error ())
    | AddHighscore ( String, Float )
    | NameChanged String


addCmdNone model =
    ( model, Cmd.none )


gearToMetersPerSecond gear =
    gear + 1 |> toFloat |> logBase 1.2 |> (*) 3


metersPerSecondToKph =
    3.6


startingSecondsLeft =
    30


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

        NewGame ->
            newModel (Random.step (Random.int 0 100000) model.randomSeed |> Tuple.first) model.highscores model.name |> (\a -> ( a, getHighscores ))

        GetHighscores result ->
            case result of
                Ok value ->
                    { model | highscores = Just value } |> addCmdNone

                Err error ->
                    model |> addCmdNone

        SetHighscores _ ->
            newModel (Random.step (Random.int 0 100000) model.randomSeed |> Tuple.first) model.highscores model.name |> (\a -> ( a, getHighscores ))

        NameChanged name ->
            { model | name = name |> String.toList |> List.take 12 |> String.fromList } |> addCmdNone

        AddHighscore ( name, value ) ->
            ( model, setHighscores (model.highscores |> Maybe.withDefault [] |> (::) ( name, value ) |> List.sortBy (\( _, x ) -> -x) |> List.take 10) )


setHighscores _ =
    Cmd.none


getHighscores =
    Cmd.none


framesPerSecond =
    60


secondsPerStep =
    1000 / framesPerSecond


nextCheckpoint : Float -> Float
nextCheckpoint metersLeft =
    [ 500, 1500, 3000, 5000, 8000, 11000, 15000, 20000, 25000, 30000 ]
        |> List.dropWhile (\a -> a < metersLeft)
        |> List.head
        |> Maybe.withDefault 1000000


step : Model -> ( Model, Cmd Msg )
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
        , targetGearShiftIndex =
            if model.secondsLeft <= 0 then
                model.targetGearShiftIndex

            else if (KeyHelper.arrowPressed model |> Just) == (GearShift.nextGearDirection model |> Maybe.map Direction.toPoint) then
                model.targetGearShiftIndex + 1

            else if (KeyHelper.arrowPressed model |> Just) == (GearShift.previousGearDirection model |> Maybe.map Direction.toPoint) then
                model.targetGearShiftIndex - 1

            else
                model.targetGearShiftIndex
        , currentGearShiftIndex =
            --(model.currentGearShiftIndex + toFloat model.targetGearShiftIndex) / 2
            if model.currentGearShiftIndex < toFloat model.targetGearShiftIndex then
                model.currentGearShiftIndex + 0.01

            else
                model.currentGearShiftIndex - 0.01
        , started =
            if GearShift.currentGear model > 0 then
                True

            else
                model.started
        , npcCars =
            model.npcCars
                |> List.filter
                    (\npcCar -> npcCar.metersLeft < model.metersLeft + 300 && npcCar.metersLeft > model.metersLeft - roadMetersVisible - 50)
                |> List.map
                    (\npcCar -> { npcCar | metersLeft = npcCar.metersLeft - npcCar.metersPerSecond / secondsPerStep })
        , metersPerSecond =
            (if model.secondsLeft <= 0 then
                model.metersPerSecond - 0.1 - model.metersPerSecond * 0.01

             else
                model.metersPerSecond + 0.1
            )
                |> clamp 0 (model |> GearShift.currentGear |> gearToMetersPerSecond)
        , previousKeys = model.keys
        , secondsLeft =
            if model.started then
                model.secondsLeft - 0.016

            else
                startingSecondsLeft
        , time = model.time + 0.016
    }
        |> (\model1 ->
                if nextCheckpoint -model.metersLeft /= nextCheckpoint -model1.metersLeft then
                    { model1 | secondsLeft = model1.secondsLeft + 10, checkpointReachedTime = model1.time }

                else
                    model1
           )
        |> (\model1 ->
                if model1.secondsLeft <= 0 || model1.time - 0.6 < model1.lastCarAddedTime then
                    model1

                else
                    addRandomCar model1
           )
        |> handleCollision
        |> (\( model1, cmd ) ->
                ( model1
                , Cmd.batch
                    (cmd
                        :: (if model.started == False && model1.started == True then
                                [ playSound "astrix_on_mushrooms.ogg" ]

                            else
                                []
                           )
                        ++ (if model.secondsLeft > 0 && model1.secondsLeft <= 0 then
                                [ getHighscores, stopSound "astrix_on_mushrooms.ogg" ]

                            else
                                []
                           )
                    )
                )
           )


handleCollision : Model -> ( Model, Cmd Msg )
handleCollision model =
    let
        npcCarTuples =
            List.map
                (\npcCar ->
                    if
                        not npcCar.destroyed
                            && (abs (toFloat npcCar.lane - model.currentLane) |> (\a -> a < 0.8))
                            && (abs (npcCar.metersLeft - model.metersLeft + 385 {- hacky offset -}) < 30)
                    then
                        ( { npcCar
                            | destroyed = True
                            , metersPerSecond = npcCar.metersPerSecond / 2
                          }
                        , True
                        )

                    else
                        ( npcCar, False )
                )
                model.npcCars

        hasCollided =
            List.any (\( _, collided ) -> collided) npcCarTuples
    in
    ( { model
        | npcCars = npcCarTuples |> List.map (\( npcCar, _ ) -> npcCar)
        , targetGearShiftIndex =
            if hasCollided then
                model.targetGearShiftIndex * 2 // 3 |> max GearShift.shiftsPerGear

            else
                model.targetGearShiftIndex
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


stopSound : String -> Cmd msg
stopSound soundName =
    Ports.StopSound { soundName = soundName }
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
        []


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
                        img
                            ([ (if npcCar.destroyed then
                                    Images.npcCarDead.source

                                else
                                    Images.npcCar.source
                               )
                                |> src
                             ]
                                ++ Helper.positionAndSize
                                    (getRoadPos
                                        model
                                        ((toFloat npcCar.lane + 0.5) / laneCount)
                                        distanceT
                                        |> Point2.add (Point2.map ((*) -0.5) imageSize)
                                    )
                                    imageSize
                            )
                            []
                    )
                |> div []
    in
    div ([ style "overflow" "hidden", style "font-family" "Arial" ] ++ Helper.positionAndSize Point2.zero screenSize)
        [ backgroundView Point2.zero screenSize model
        , npcCars
        , Helper.imageView
            { x = screenSize.x / 2 - toFloat Images.playerCar.size.x / 2
            , y = screenSize.y - 150
            }
            Images.playerCar
        , GearShift.view { x = 800, y = 100 } { x = 290, y = 290 } model
        , speedometerView { x = 50, y = 20 } model
        , div
            ([ style "font-size" "50px"
             , style "color"
                (if model.secondsLeft < 10 then
                    "red"

                 else
                    "black"
                )
             ]
                ++ Helper.positionAndSize { x = screenSize.x / 2 - 200, y = 20 } { x = 400, y = 300 }
            )
            [ "Time Left: " ++ (model.secondsLeft |> max 0 |> Round.round 2) |> text ]
        , if model.started then
            div [] []

          else
            div
                ([ style "color" "white", style "font-size" "50px", style "text-align" "center" ]
                    ++ Helper.positionAndSize { x = screenSize.x / 2 - 300, y = 640 } { x = 600, y = 300 }
                )
                [ text "← Steer with A and D →" ]
        , if model.started then
            div [] []

          else
            highscoreTable Nothing model.highscores
        , if model.secondsLeft <= 0 && model.metersPerSecond <= 0 then
            let
                newRecord =
                    model.highscores
                        |> Maybe.map (\a -> a |> List.any (\( _, value ) -> value < -model.metersLeft) |> (||) (List.length a < 10))
                        |> Maybe.withDefault False
            in
            div
                ([ style "text-align" "center", style "font-size" "50px", style "background-color" "#FFFFFFAA" ]
                    ++ Helper.positionAndSize
                        { x = screenSize.x / 2 - 375, y = 300 }
                        { x = 750, y = 250 }
                )
                [ text "TIME IS UP!"
                , Html.br [] []
                , text ("You traveled " ++ (-model.metersLeft / 1000 |> Round.round 2) ++ " kilometers!")
                , if newRecord then
                    div []
                        [ text "NEW RECORD! "
                        , Html.input
                            [ Html.Attributes.type_ "textbox"
                            , Html.Attributes.value model.name
                            , Html.Events.onInput NameChanged
                            , style "font-size" "30px"
                            ]
                            [ text "Your name" ]
                        ]

                  else
                    div [] []
                , Html.button
                    [ style "font-size" "40px"
                    , style "margin" "10px"
                    , onClick
                        (if newRecord then
                            AddHighscore ( model.name, -model.metersLeft )

                         else
                            NewGame
                        )
                    ]
                    [ text
                        (if newRecord then
                            "Upload Score"

                         else
                            "Play again?"
                        )
                    ]
                ]

          else
            div [] []
        , if model.checkpointReachedTime + 3 > model.time then
            div
                ([ style "text-align" "center", style "font-size" "50px" ]
                    ++ Helper.positionAndSize
                        { x = screenSize.x / 2 - 375, y = 300 }
                        { x = 750, y = 190 }
                )
                [ text "Checkpoint!! (+10 seconds)" ]

          else
            div [] []
        ]


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
    div ([ style "font-size" "30px" ] ++ Helper.positionAndSize position { x = 400, y = 300 })
        [ model.metersPerSecond
            * metersPerSecondToKph
            |> Round.round 1
            |> (\a -> "KPH: " ++ a)
            |> text
        , Html.br [] []
        , -model.metersLeft
            / 1000
            |> Round.round 2
            |> (\a -> "Distance: " ++ a)
            |> text
        , Html.br [] []
        , GearShift.currentGear model
            |> String.fromInt
            |> (\a -> "Gear: " ++ a)
            |> text
        , Html.br [] []
        , (nextCheckpoint -model.metersLeft + model.metersLeft)
            / 1000
            |> Round.round 2
            |> (\a -> "Next checkpoint: " ++ a)
            |> text
        ]


highscoreTable : Maybe Int -> Maybe (List ( String, Float )) -> Html msg
highscoreTable newScore maybeScoreList =
    let
        rows =
            case maybeScoreList of
                Just scoreList ->
                    scoreList
                        |> List.sortBy (\( _, value ) -> -value)
                        |> List.take 10
                        |> List.indexedMap
                            (\index ( name, value ) ->
                                div
                                    ((if Just index == newScore then
                                        [ style "background-color" "#FFFF00AA" ]

                                      else
                                        []
                                     )
                                        ++ [ style "font-size" "20px" ]
                                    )
                                    [ div
                                        [ style "float" "left" ]
                                        [ (String.fromInt (index + 1) ++ ". " ++ name) |> text ]
                                    , div
                                        [ style "text-align" "right" ]
                                        [ value / 1000 |> Round.round 2 |> (\a -> a ++ "km") |> text ]
                                    ]
                            )

                Nothing ->
                    [ div [ style "font-size" "20px" ] [ text "Failed to load :(" ] ]
    in
    div ([ style "background-color" "#FFFFFFAA", style "margin" "5px" ] ++ Helper.positionAndSize { x = 20, y = 270 } { x = 300, y = 440 })
        (div [ style "font-size" "40px" ] [ text "Highscores" ] :: rows)


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
            ([ style "background-color" "green" ]
                ++ Helper.positionAndSize
                    { x = 0, y = screenSize.y / 2 }
                    { x = screenSize.x, y = screenSize.y / 2 }
            )
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


decodeHighscoreRow : Decode.Decoder ( String, Float )
decodeHighscoreRow =
    Decode.map2 (\a b -> ( a, b ))
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.float)


decodeHighscores : Decode.Decoder (List ( String, Float ))
decodeHighscores =
    Decode.list decodeHighscoreRow


encodeHighscores highscores =
    Encode.list encodeHighscoreRow highscores


encodeHighscoreRow ( name, value ) =
    Encode.object [ ( "name", Encode.string name ), ( "value", Encode.float value ) ]



---- SUBSCRIPTION ----


subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , if model.pause then
            Sub.none

          else
            Time.every secondsPerStep Step

        --Time.every 2000 Step
        ]
