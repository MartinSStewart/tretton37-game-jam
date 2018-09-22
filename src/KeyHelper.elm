module KeyHelper exposing (..)

import Point2 exposing (Point2)
import Keyboard exposing (Key(..))
import Keyboard.Arrows


type alias KeyboardState r =
    { r
        | previousKeys : List Key
        , keys : List Key
    }


isPressed : KeyboardState a -> Key -> Bool
isPressed model key =
    List.any ((==) key) model.keys && (List.any ((==) key) model.previousKeys |> not)


isDown : KeyboardState a -> Key -> Bool
isDown model key =
    List.any ((==) key) model.keys


isReleased : KeyboardState a -> Key -> Bool
isReleased model key =
    List.any ((==) key) model.previousKeys


arrowPressed : KeyboardState a -> Point2 Int
arrowPressed model =
    (if Keyboard.Arrows.arrows model.keys == Keyboard.Arrows.arrows model.previousKeys then
        Point2.zero

     else
        Keyboard.Arrows.arrows model.keys
    )
        |> Point2.mirrorY
