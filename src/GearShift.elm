module GearShift exposing (..)

import Point2 exposing (Point2)
import Direction exposing (Direction(..))
import Random
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, style)
import Images
import Helper
import List.Extra as List
import Set exposing (Set)


type alias GearShiftState r =
    { r
        | gearShiftPath : List Direction
        , gearShiftIndex : Int
    }


maxGear =
    25

shiftsPerGear =
    3


currentGear : GearShiftState a -> Int
currentGear model =
    model.gearShiftIndex // shiftsPerGear


moveInPath : List Direction -> Point2 Int
moveInPath path =
    List.foldl (\a b -> a |> Direction.toPoint |> Point2.add b) Point2.zero path

pointsInPath : List Direction -> List (Point2 Int)
pointsInPath path =
    List.range 0 (List.length path |> (+) -1)
        |> List.map (\index -> List.take index path |> moveInPath)


getGearShiftPath : Random.Seed -> List Direction
getGearShiftPath seed =
    getGearShiftPathHelper seed []

getGearShiftPathHelper : Random.Seed -> List Direction -> List Direction
getGearShiftPathHelper seedRandom path =
    let
        existingPoints =
            pointsInPath (List.reverse path)

        (nextDirection, seedRandom1) =
            Random.step
                (Random.uniform Left [ Left, Up, Right, Down ])
                seedRandom

        nextPoint =
            moveInPath (nextDirection :: path)
    in
        if List.length path > (maxGear * shiftsPerGear) then
            path
        else if List.any (\a -> nextPoint == a) existingPoints then
            getGearShiftPathHelper seedRandom1 (List.drop 1 path)
        else
            getGearShiftPathHelper seedRandom1 (nextDirection :: path)


nextGearDirection : GearShiftState a -> Maybe Direction
nextGearDirection model =
    List.getAt model.gearShiftIndex model.gearShiftPath


previousGearDirection : GearShiftState a -> Maybe Direction
previousGearDirection model =
    model.gearShiftPath
        |> List.getAt (model.gearShiftIndex - 1)
        |> Maybe.map Direction.reverse


view : Point2 Float -> Point2 Float -> GearShiftState a -> Html msg
view position size model =
    let
        gearUpText =
            if currentGear model == 0 then
                div ([ style "font-size" "50px", style "font-family" "Consolas, Arial" ]
                        ++ Helper.positionAndSize
                            { x = -350, y = 55 }
                            { x = 450, y = 30 }
                    )
                    [ text "Car idle, gear up with arrow keys! →" ]
            else
                div [] []

        shiftLength = 100

        centerPoint =
            Point2.map ((*) 0.5) size

        getPath path index incrementBy drawGear =
            List.foldl
                (\direction ( html, position1, index1 ) ->
                    (   if drawGear then
                            let
                                gearIndex =
                                    (model.gearShiftIndex + index1) // shiftsPerGear
                            in

                            if modBy shiftsPerGear (model.gearShiftIndex + index1) == 0 then
                                drawGearNumber (Point2.add position1 centerPoint) shiftLength direction index1 gearIndex :: html
                            else
                                html

                        else
                            drawDirection (Point2.add position1 centerPoint) shiftLength direction index1 :: html
                    , moveInDirection direction shiftLength position1
                    , index1 + incrementBy
                    )
                )
                ( [], Point2.zero, index )
                path
                |> (\( html, _, _ ) -> html)

        forwardPath =
            getPath (model.gearShiftPath |> List.drop model.gearShiftIndex) 0 1 False

        reversePath =
            getPath
                (model.gearShiftPath
                    |> List.take model.gearShiftIndex
                    |> List.reverse
                    |> List.map Direction.reverse
                )
                -1
                -1
                False

        forwardGearNumbers =
            getPath (model.gearShiftPath |> List.drop model.gearShiftIndex) 0 1 True

        reverseGearNumbers =
            getPath
                (model.gearShiftPath
                    |> List.take model.gearShiftIndex
                    |> List.reverse
                    |> List.map Direction.reverse
                )
                -1
                -1
                True
    in
    div (Helper.positionAndSize position size)
        [ div (style "overflow" "hidden" :: Helper.positionAndSize Point2.zero size)
            (reversePath ++ forwardPath ++ forwardGearNumbers)
        , Helper.imageView { x = 135, y = 19} Images.stickShift
        , gearUpText
        ]


drawDirection : Point2 Float -> Float -> Direction -> Int -> Html msg
drawDirection position length direction stepsFromCurrentGear =
    let
        thickness =
            20.0

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
            "#333333FF"
    in
    div
        []
        [ div
            ([ style "background-color" color ] ++ Helper.positionAndSize position1 size)
            []
        ,   if stepsFromCurrentGear == 0 then
                div
                    ([ style "background-color" "green" ] ++ Helper.positionAndSize position1 size)
                    []
            else
                div [] []
        ]


drawGearNumber : Point2 Float -> Float -> Direction -> Int -> Int -> Html msg
drawGearNumber position length direction stepsFromCurrentGear gearIndex =
    let
        thickness =
            20.0

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
            "#333333FF"
    in
    div
        ([ style "color" "white" ] ++ Helper.positionAndSize (Point2.add position { x = -4, y = -6 }) size)
        [ gearIndex |> String.fromInt |> text]


moveInDirection : Direction -> number -> Point2 number -> Point2 number
moveInDirection direction length point =
    Direction.toPoint direction
        |> Point2.map ((*) length)
        |> Point2.add point
