module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Http
import Keyboard
import Evergreen.V1.Main as Main
import Time
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , model : Main.Model
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MainMsg Main.Msg
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend