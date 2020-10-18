module Shared exposing (FrontendMsg(..), Model, addHighscore, frontendMsgCodec, highscoreCodec)

import Codec.Bytes exposing (Codec)


type alias Model =
    { highscores : List ( String, Float ) }


type FrontendMsg
    = RequestHighscores
    | NewHighscore String Float


addHighscore : String -> Float -> Model -> Model
addHighscore name score model =
    { model
        | highscores =
            ( name, score )
                :: model.highscores
                |> List.sortBy (Tuple.second >> negate)
                |> List.take 10
    }


frontendMsgCodec : Codec FrontendMsg
frontendMsgCodec =
    Codec.Bytes.custom
        (\requestHighscoreEncoder newHighscoreEncoder value ->
            case value of
                RequestHighscores ->
                    requestHighscoreEncoder

                NewHighscore name score ->
                    newHighscoreEncoder name score
        )
        |> Codec.Bytes.variant0 0 RequestHighscores
        |> Codec.Bytes.variant2 1 NewHighscore Codec.Bytes.string Codec.Bytes.float64
        |> Codec.Bytes.buildCustom


highscoreCodec : Codec (List ( String, Float ))
highscoreCodec =
    Codec.Bytes.list (Codec.Bytes.tuple Codec.Bytes.string Codec.Bytes.float64)
