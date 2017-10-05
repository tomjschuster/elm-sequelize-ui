module Utils.Handlers
    exposing
        ( onEnter
        , onEscape
        , onKeyDown
        , onPreventDefaultClick
        )

--import Json.Decode.Pipeline exposing

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Events exposing (defaultOptions, on, onWithOptions)
import Json.Decode as JD exposing (Decoder)
import Utils.Keys as Keys exposing (Key(..))


onKeyDown : List Key -> msg -> Attribute msg
onKeyDown keys msg =
    on "keydown" (keyCodeDecoder keys |> JD.andThen (msgBoolDecoder msg))


customOnKeyDown : (Key -> Maybe msg) -> Attribute msg
customOnKeyDown toMsg =
    on "keydown"
        (JD.field "keyCode" JD.int
            |> JD.andThen
                (Keys.fromKeyCode
                    >> Maybe.andThen toMsg
                    >> Maybe.map JD.succeed
                    >> Maybe.withDefault (JD.fail "invalid key")
                )
        )


onEnter : msg -> Attribute msg
onEnter =
    onKeyDown [ Enter ]


onEscape : msg -> Attribute msg
onEscape =
    onKeyDown [ Escape ]


isKeyCode : List Key -> Int -> Bool
isKeyCode keys =
    Keys.fromKeyCode
        >> Maybe.map (flip List.member keys)
        >> Maybe.withDefault False


keyCodeDecoder : List Key -> Decoder Bool
keyCodeDecoder keys =
    JD.field "keyCode" JD.int
        |> JD.map (isKeyCode keys)


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (notModifierKeyDecoder
            |> JD.andThen (msgBoolDecoder message)
        )


notModifierKeyDecoder : Decoder Bool
notModifierKeyDecoder =
    JD.map2
        (\x y -> not (x || y))
        (JD.field "ctrlKey" JD.bool)
        (JD.field "metaKey" JD.bool)


msgBoolDecoder : msg -> Bool -> Decoder msg
msgBoolDecoder msg preventDefault =
    case preventDefault of
        True ->
            JD.succeed msg

        False ->
            JD.fail "Normal link"
