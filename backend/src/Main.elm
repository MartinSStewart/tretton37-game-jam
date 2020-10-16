module Main exposing (Msg(..), main, update)

import Codec.Bytes exposing (Codec)
import Dict exposing (Dict)
import Http.Server.LowLevel as HSL
import MainLogic exposing (FrontendMsg(..), Model)
import Simplex


type Msg
    = GetRequest HSL.HttpRequest


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetRequest req ->
            case Codec.Bytes.decodeValue MainLogic.frontendMsgCodec req.body of
                Just RequestHighscores ->
                    ( model
                    , HSL.respond
                        { requestId = req.requestId
                        , status = 200
                        , headers = [ ( "Content-Type", "application/octet-stream" ) ]
                        , body =
                            Codec.Bytes.encodeToValue MainLogic.highscoreCodec model.highscores
                        }
                    )

                Just (NewHighscore name score) ->
                    let
                        newModel =
                            MainLogic.addHighscore name score model
                    in
                    ( newModel
                    , HSL.respond
                        { requestId = req.requestId
                        , status = 200
                        , headers = [ ( "Content-Type", "application/octet-stream" ) ]
                        , body =
                            Codec.Bytes.encodeToValue MainLogic.highscoreCodec newModel.highscores
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )


main =
    Simplex.new
        { init = \() -> ( { highscores = [] }, Cmd.none )
        , update = update
        , subscriptions = \m -> HSL.subscribe GetRequest
        }
