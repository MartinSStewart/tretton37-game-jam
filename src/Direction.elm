module Direction exposing (..)

import Point2 exposing (..)
import Random


type Direction
    = Left
    | Right
    | Up
    | Down


toPoint : Direction -> Point2 number
toPoint direction =
    case direction of
        Left ->
            Point2 -1 0

        Right ->
            Point2 1 0

        Up ->
            Point2 0 -1

        Down ->
            Point2 0 1


reverse : Direction -> Direction
reverse direction =
    case direction of
        Left ->
            Right

        Right ->
            Left

        Up ->
            Down

        Down ->
            Up

turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        Left ->
            Up

        Right ->
            Down

        Up ->
            Right

        Down ->
            Left

random : Random.Generator Direction
random =
    Random.int 0 3
        |> Random.map
            (\a ->
                case a of
                    0 ->
                        Right

                    1 ->
                        Up

                    2 ->
                        Left

                    _ ->
                        Down
            )
