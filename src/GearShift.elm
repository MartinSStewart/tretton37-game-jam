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
    4


currentGear : GearShiftState a -> Int
currentGear model =
    model.gearShiftIndex // shiftsPerGear


moveInPath : List Direction -> Point2 Int
moveInPath path =
    List.foldl (\a b -> a |> Direction.toPoint |> Point2.add b) Point2.zero path


getGearShiftPath : Random.Seed -> List Direction
getGearShiftPath seed =
    getGearShiftPathHelper seed (maxGear * shiftsPerGear) Set.empty []


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
                div ([ style "font-size" "50px", style "font-family" "Consolas, Arial" ] ++ Helper.positionAndSize { x = -350, y = 70 } { x = 450, y = 30 })
                    [ text "Car idle, gear up with arrow keys! →" ]
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
    div (Helper.positionAndSize position size)
        [ gearUpText
        , div (style "overflow" "hidden" :: Helper.positionAndSize Point2.zero size)
            (reversePath ++ forwardPath)
        ]


drawDirection : Point2 Float -> Float -> Direction -> Int -> Html msg
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
    div ([ style "background-color" color ] ++ Helper.positionAndSize position1 size)
        []


moveInDirection : Direction -> number -> Point2 number -> Point2 number
moveInDirection direction length point =
    Direction.toPoint direction
        |> Point2.map ((*) length)
        |> Point2.add point
