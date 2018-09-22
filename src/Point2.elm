module Point2 exposing
    ( Point2
    , add
    , map
    , mirrorX
    , mirrorY
    , mult
    , zero
    )


type alias Point2 number =
    { x : number, y : number }


zero : Point2 number
zero =
    Point2 0 0


add : Point2 number -> Point2 number -> Point2 number
add pointA pointB =
    { x = pointA.x + pointB.x, y = pointA.y + pointB.y }


mult : Point2 number -> Point2 number -> Point2 number
mult pointA pointB =
    { x = pointA.x * pointB.x, y = pointA.y * pointB.y }


map : (number -> number2) -> Point2 number -> Point2 number2
map mapFunc point =
    { x = mapFunc point.x, y = mapFunc point.y }


mirrorY : Point2 number -> Point2 number
mirrorY point =
    { x = point.x, y = -point.y }


mirrorX : Point2 number -> Point2 number
mirrorX point =
    { x = -point.x, y = point.y }
