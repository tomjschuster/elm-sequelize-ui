module Views.DataTypeSelect exposing (Config, view)

import Data.DataType as DataType exposing (DataType)
import Html as Html exposing (Html, div, input, label, option, select, text)
import Html.Attributes exposing (checked, name, selected, type_, value)
import Utils.Handlers exposing (onChangeBool, onChangeInt, onIntInput)


type alias Config msg =
    { handleChange : Maybe Int -> msg
    , handleSizeInput : Maybe Int -> msg
    , handlePrecisionInput : Maybe Int -> Maybe Int -> msg
    , handleTimezoneCheck : Bool -> msg
    }


view : Config msg -> DataType -> DataType.Modifier -> Html msg
view config dataType modifier =
    div [] (children config dataType modifier)


children : Config msg -> DataType -> DataType.Modifier -> List (Html msg)
children config dataType modifier =
    modifierView config modifier
        |> Maybe.map (List.singleton >> (::) (dataTypeSelect config dataType))
        |> Maybe.withDefault [ dataTypeSelect config dataType ]


dataTypeSelect : Config msg -> DataType -> Html msg
dataTypeSelect config dataType =
    select
        [ onChangeInt config.handleChange ]
        (option [] [ text "Data Type" ] :: List.map (selectOption config dataType) DataType.all)


selectOption : Config msg -> DataType -> DataType -> Html msg
selectOption config currentType dataType =
    option
        [ selected (currentType == dataType)
        , value (DataType.toId dataType |> toString)
        ]
        [ text (DataType.toString dataType) ]


modifierView : Config msg -> DataType.Modifier -> Maybe (Html msg)
modifierView config modifier =
    case modifier of
        DataType.NoModifier ->
            Nothing

        DataType.Size size ->
            Just (sizeInput config.handleSizeInput size)

        DataType.Precision precision decimals ->
            Just (precisionInput config.handlePrecisionInput precision decimals)

        DataType.WithTimezone withTimezone ->
            Just (timezoneCheckbox config.handleTimezoneCheck withTimezone)


sizeInput : (Maybe Int -> msg) -> Maybe Int -> Html msg
sizeInput handleSizeInput size =
    div []
        [ label []
            [ text "Size" ]
        , input
            [ type_ "number"
            , value (maybeIntToString size)
            , onIntInput handleSizeInput
            ]
            []
        ]


precisionInput : (Maybe Int -> Maybe Int -> msg) -> Maybe Int -> Maybe Int -> Html msg
precisionInput handlePrecisionInput precision decimals =
    div []
        [ div []
            [ label [] [ text "Precision" ]
            , input
                [ type_ "number"
                , onIntInput (flip handlePrecisionInput decimals)
                , value (maybeIntToString precision)
                ]
                []
            ]
        , div []
            [ label [] [ text "Decimals" ]
            , input
                [ type_ "number"
                , onIntInput (handlePrecisionInput precision)
                , value (maybeIntToString decimals)
                ]
                []
            ]
        ]


timezoneCheckbox : (Bool -> msg) -> Bool -> Html msg
timezoneCheckbox handleTimezoneCheck isChecked =
    label []
        [ text "With Timezone"
        , input
            [ type_ "checkbox"
            , checked isChecked
            , onChangeBool handleTimezoneCheck
            ]
            []
        ]


maybeIntToString : Maybe Int -> String
maybeIntToString =
    Maybe.map toString >> Maybe.withDefault ""
