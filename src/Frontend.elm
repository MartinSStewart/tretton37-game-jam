module Frontend exposing (app, init, update, updateFromBackend, view)

import Audio exposing (AudioCmd)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Lamdera
import Main
import Types exposing (..)
import Url


app =
    Audio.lamderaFrontendWithAudio
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        , audio = always Audio.silence
        , audioPort = { toJS = always Cmd.none, fromJS = always Sub.none }
        }


init : Url.Url -> Nav.Key -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
init url key =
    ( { key = key, model = Main.newModel 321321 Nothing "No name" }
    , Cmd.none
    , Audio.cmdNone
    )


update : FrontendMsg_ -> FrontendModel_ -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    , Audio.cmdNone
                    )

                External url ->
                    ( model
                    , Nav.load url
                    , Audio.cmdNone
                    )

        UrlChanged url ->
            ( model, Cmd.none, Audio.cmdNone )

        MainMsg mainMsg ->
            let
                ( mainModel, cmd ) =
                    Main.update mainMsg model.model
            in
            ( { model | model = mainModel }, Cmd.map MainMsg cmd, Audio.cmdNone )

        NoOpFrontendMsg ->
            ( model, Cmd.none, Audio.cmdNone )


updateFromBackend : ToFrontend -> FrontendModel_ -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
updateFromBackend msg model =
    ( model, Cmd.none, Audio.cmdNone )


subscriptions model =
    Main.subscriptions model.model |> Sub.map MainMsg


view model =
    { title = ""
    , body =
        [ Main.view model.model |> Html.map MainMsg
        ]
    }
