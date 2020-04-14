module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Main
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , model : Main.Model
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | MainMsg Main.Msg
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
