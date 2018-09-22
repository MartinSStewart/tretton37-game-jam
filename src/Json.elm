module Json exposing (..)

import Json.Encode as Enc exposing (object)
import Json.Decode as Dec exposing (andThen, field)
import Ports exposing (..)


encodePortOutMsg : PortOutMsg -> Enc.Value
encodePortOutMsg a =
    case a of
        PlaySound a1 ->
            object
                [ ( "Constructor", Enc.string "PlaySound" )
                , ( "A1", encodeRecord_soundName_String_loop_Bool_ a1 )
                ]

        StopSound a1 ->
            object
                [ ( "Constructor", Enc.string "StopSound" )
                , ( "A1", encodeRecord_soundName_String_ a1 )
                ]


encodeRecord_soundName_String_ : { a | soundName : String } -> Enc.Value
encodeRecord_soundName_String_ a =
    object
        [ ( "soundName", Enc.string a.soundName )
        ]


encodeRecord_soundName_String_loop_Bool_ :
    { a | loop : Bool, soundName : String }
    -> Enc.Value
encodeRecord_soundName_String_loop_Bool_ a =
    object
        [ ( "soundName", Enc.string a.soundName )
        , ( "loop", Enc.bool a.loop )
        ]
