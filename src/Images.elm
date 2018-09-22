module Images exposing (..)

import Point2 exposing (..)


type alias Image =
    { source : String
    , size : Point2 Int
    }


playerCar : Image
playerCar =
    { source = "car.png"
    , size = { x = 342, y = 260 }
    }

npcCar : Image
npcCar =
    { source = "npc_car.png"
    , size = { x = 342, y = 260 }
    }
