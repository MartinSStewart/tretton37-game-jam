port module Ports exposing (..)


import Json.Encode


port portOut : Json.Encode.Value -> Cmd msg


type PortOutMsg
    = PlaySound { soundName : String, loop : Bool }
    | StopSound { soundName : String }


-- PlaySound { soundName = "explosion.mp3", loop = False }
--         |> Json.encodePortOutMsg
--         |> portOut
