module Direction exposing (..)

import Point2 exposing (..)

type Direction = Left | Right | Up | Down

directionToPoint : Direction -> Point2 number
directionToPoint direction =
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
