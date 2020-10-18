module Main exposing (Msg(..), main, update)

import Bytes.Encode
import Codec.Bytes exposing (Codec)
import Http.Server.LowLevel as HSL
import Shared exposing (FrontendMsg(..), Model)
import Simplex


type Msg
    = GetRequest_ HSL.HttpRequest
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetRequest_ req ->
            if req.method == HSL.Options then
                ( model
                , HSL.respond
                    { requestId = req.requestId
                    , status = 200
                    , headers = [ ( "Content-Type", "text/plain" ) ]
                    , body = Bytes.Encode.sequence [] |> Bytes.Encode.encode
                    }
                )

            else
                case Codec.Bytes.decodeValue Shared.frontendMsgCodec req.body of
                    Just RequestHighscores ->
                        ( model
                        , HSL.respond
                            { requestId = req.requestId
                            , status = 200
                            , headers = [ ( "Content-Type", "application/octet-stream" ) ]
                            , body =
                                Codec.Bytes.encodeToValue Shared.highscoreCodec model.highscores
                            }
                        )

                    Just (NewHighscore name score) ->
                        let
                            newModel =
                                Shared.addHighscore name score model
                        in
                        ( newModel
                        , HSL.respond
                            { requestId = req.requestId
                            , status = 200
                            , headers = [ ( "Content-Type", "application/octet-stream" ) ]
                            , body =
                                Codec.Bytes.encodeToValue Shared.highscoreCodec newModel.highscores
                            }
                        )

                    Nothing ->
                        ( model
                        , HSL.respond
                            { requestId = req.requestId
                            , status = 500
                            , headers = [ ( "Content-Type", "text/plain" ) ]
                            , body = req.body
                            }
                        )

        NoOp ->
            ( model, Cmd.none )


init : Model
init =
    { highscores = [] }


main : Simplex.BackendProgram () Model Msg ( Msg, Model )
main =
    Simplex.zeroDowntimeMigration
        { update = update
        , subscriptions = \m -> HSL.subscribe GetRequest_
        , migrate = { model = \_ _ -> Ok init, msg = \_ _ -> Ok NoOp }
        }
