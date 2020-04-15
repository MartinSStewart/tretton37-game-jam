module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendModel_, FrontendMsg_(..), ToBackend(..), ToFrontend(..))

import Audio
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Main
import Url exposing (Url)


type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type alias FrontendModel_ =
    { key : Key
    , model : Main.Model
    }


type alias BackendModel =
    { message : String
    }


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendMsg_
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
