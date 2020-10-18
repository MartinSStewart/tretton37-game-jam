module Tests exposing (all)

import Codec.Bytes
import Expect
import Shared exposing (FrontendMsg(..))
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Codec" <|
            \_ ->
                Codec.Bytes.encodeToValue Shared.frontendMsgCodec RequestHighscores
                    |> Codec.Bytes.decodeValue Shared.frontendMsgCodec
                    |> Expect.equal (Just RequestHighscores)
        ]
