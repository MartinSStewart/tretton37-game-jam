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


npcCarDead : Image
npcCarDead =
    { source = "npc_car_dead.png"
    , size = { x = 342, y = 260 }
    }


stickShift : Image
stickShift =
    { source = "stick_shift.png"
    , size = { x = 130, y = 140 }
    }
