module Utils.Handlers
    exposing
        ( customOnKeyDown
        , onChangeBool
        , onChangeId
        , onChangeInt
        , onEnter
        , onEscape
        , onIntInput
        , onKeyDown
        , onPreventDefaultClick
        )

import Html exposing (Attribute)
import Html.Events exposing (Options, defaultOptions, on, onWithOptions)
import Json.Decode as JD exposing (Decoder)
import Utils.Keys as Keys exposing (Key(..))


-- KEYS


onKeyDown : List Key -> msg -> Attribute msg
onKeyDown keys msg =
    on "keydown" (keyCodeDecoder keys |> JD.andThen (msgBoolDecoder msg))


customOnKeyDown : (Key -> Maybe msg) -> Attribute msg
customOnKeyDown toMsg =
    onWithOptions "keydown"
        (Options True True)
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



-- CLICK


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (notModifierKeyDecoder
            |> JD.andThen (msgBoolDecoder message)
        )



-- CHANGE


onChangeId : ({ a | id : Int } -> msg) -> List { a | id : Int } -> Attribute msg
onChangeId toMsg data =
    on "change" (JD.andThen (Maybe.andThen (flip findInList data >> Maybe.map toMsg) >> failOnNothingDecoder) targetValueIntDecoder)


findInList : Int -> List { a | id : Int } -> Maybe { a | id : Int }
findInList id =
    List.filter (.id >> (==) id) >> List.head


onChangeInt : (Maybe Int -> msg) -> Attribute msg
onChangeInt toMsg =
    on "change" (JD.map toMsg targetValueIntDecoder)


onChangeBool : (Bool -> msg) -> Attribute msg
onChangeBool toMsg =
    on "change" (targetCheckedDecoder |> JD.map toMsg)



-- INPUT


onIntInput : (Maybe Int -> msg) -> Attribute msg
onIntInput toMsg =
    on "input" (targetValueIntDecoder |> JD.map toMsg)



-- HELPER DECODERS


keyCodeDecoder : List Key -> Decoder Bool
keyCodeDecoder keys =
    JD.field "keyCode" JD.int
        |> JD.map (Keys.isKeyCode keys)


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


targetValueIntDecoder : Decoder (Maybe Int)
targetValueIntDecoder =
    JD.field "target"
        (JD.field "value"
            (JD.string
                |> JD.andThen
                    (stringToMaybe
                        >> Maybe.map (String.toInt >> failOnErrorDecoder >> JD.map Just)
                        >> Maybe.withDefault (JD.succeed Nothing)
                    )
            )
        )


targetCheckedDecoder : Decoder Bool
targetCheckedDecoder =
    JD.field "target" (JD.field "checked" JD.bool)


failOnNothingDecoder : Maybe a -> Decoder a
failOnNothingDecoder =
    Maybe.map JD.succeed >> Maybe.withDefault (JD.fail "Nothing")


failOnErrorDecoder : Result a b -> Decoder b
failOnErrorDecoder =
    Result.map JD.succeed >> Result.withDefault (JD.fail "Error")


failOnFalse : a -> Bool -> Decoder a
failOnFalse value success =
    if success then
        JD.succeed value
    else
        JD.fail "false"



-- UTILS


stringToMaybe : String -> Maybe String
stringToMaybe string =
    if string == "" then
        Nothing
    else
        Just string
