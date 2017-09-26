module Utils.Handlers exposing (onPreventDefaultClick)

import Html exposing (Attribute)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Decode as JD exposing (Decoder)


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (notModifierKeyDecoder
            |> JD.andThen (maybePreventDefault message)
        )


notModifierKeyDecoder : Decoder Bool
notModifierKeyDecoder =
    JD.map2
        (\x y -> not (x || y))
        (JD.field "ctrlKey" JD.bool)
        (JD.field "metaKey" JD.bool)


maybePreventDefault : msg -> Bool -> Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            JD.succeed msg

        False ->
            JD.fail "Normal link"
