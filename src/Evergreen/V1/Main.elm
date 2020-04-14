module Evergreen.V1.Main exposing (..)

import Evergreen.V1.Direction as Direction
import Http
import Keyboard
import Random
import Time


type alias Car = 
    { lane : Int
    , metersPerSecond : Float
    , metersLeft : Float
    , destroyed : Bool
    }


type alias Model = 
    { targetLane : Int
    , currentLane : Float
    , gearShiftPath : (List Direction.Direction)
    , currentGearShiftIndex : Float
    , targetGearShiftIndex : Int
    , secondsLeft : Float
    , metersLeft : Float
    , metersPerSecond : Float
    , npcCars : (List Car)
    , previousKeys : (List Keyboard.Key)
    , keys : (List Keyboard.Key)
    , randomSeed : Random.Seed
    , pause : Bool
    , started : Bool
    , lastCarAddedTime : Float
    , time : Float
    , checkpointReachedTime : Float
    , highscores : (Maybe (List (String, Float)))
    , name : String
    }


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg
    | Step Time.Posix
    | NewGame
    | GetHighscores (Result Http.Error (List (String, Float)))
    | SetHighscores (Result Http.Error ())
    | AddHighscore (String, Float)
    | NameChanged String