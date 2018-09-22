module Helper exposing (..)

import Point2 exposing (Point2)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, style)


debugShow : a -> Html msg
debugShow =
    Debug.toString >> text


px : Float -> String
px value =
    String.fromFloat value ++ "px"


positionAndSize : Point2 Float -> Point2 Float -> List (Html.Attribute msg)
positionAndSize position size =
    [ style "position" "absolute"
    , style "left" (px position.x)
    , style "top" (px position.y)
    , style "width" (px size.x)
    , style "height" (px size.y)
    ]