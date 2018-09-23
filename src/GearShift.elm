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
        -- if List.empty branches then
        --     getGearShiftPathHelper
        --         seedRandom
        --         availableDirections
        --         (List.drop 1 path)
        --         (List.head path |> Maybe.withDefault Left |> Direction.turnLeft)
        -- else



-- getGearShiftPathHelper : Random.Seed -> Int -> Set ( Int, Int ) -> List Direction -> Maybe (List Direction)
-- getGearShiftPathHelper seed stepsLeft set path =
--     let
--         ( direction, seed1 ) =
--             Random.step Direction.random seed
--
--         position =
--             moveInPath (direction :: path) |> (\a -> ( a.x, a.y ))
--
--         direction1 =
--             if Set.insert position set then
--                 Right
--
--             else
--                 direction
--
--         next =
--             getGearShiftPathHelper
--                 seed1
--                 (stepsLeft - 1)
--                 (Set.insert position set)
--                 (direction1 :: path)
--     in
--     if stepsLeft == 0 then
--         Just path
--     else
--         case next of
--             Just a ->
--
--             Nothing ->
--                 getGearShiftPathHelper
--                     seed1
--                     (stepsLeft - 1)
--                     (Set.insert position set)
--                     (direction1 :: path)




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
