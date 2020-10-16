module Main exposing (Msg(..), main, update)

import Bytes.Encode
import Codec.Bytes exposing (Codec)
import Http.Server.LowLevel as HSL
import MainLogic exposing (FrontendMsg(..), Model)
import Simplex


type Msg
    = GetRequest_ HSL.HttpRequest
    | NoOp


type OldMsg
    = GetRequest HSL.HttpRequest


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetRequest_ req ->
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
                    ( model
                    , HSL.respond
                        { requestId = req.requestId
                        , status = 200
                        , headers = [ ( "Content-Type", "text/plain" ) ]
                        , body = Bytes.Encode.string "Bad body" |> Bytes.Encode.encode
                        }
                    )

        NoOp ->
            ( model, Cmd.none )


init : Model
init =
    { highscores = [] }


main : Simplex.BackendProgram () Model Msg ( OldMsg, Model )
main =
    Simplex.zeroDowntimeMigration
        { update = update
        , subscriptions = \m -> HSL.subscribe GetRequest_
        , migrate = { model = \_ _ -> Ok init, msg = \_ _ -> Ok NoOp }
        }
