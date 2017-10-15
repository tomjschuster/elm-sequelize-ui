module Views.DataType.Select exposing (Config, view)

import Data.DataType as DataType exposing (DataType)
import Html as Html exposing (Attribute, Html, div, input, label, option, select, text)
import Html.Attributes exposing (checked, name, selected, type_, value)
import Utils.Handlers exposing (onChangeBool, onChangeInt, onIntInput)


type alias Config msg =
    { handleDataTypeChange : DataType -> msg
    , handleModifierChange : DataType.Modifier -> msg
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
        [ onChangeInt (handleDataTypeChange config) ]
        (dataTypeSelectChildren config dataType)


handleDataTypeChange : Config msg -> Maybe Int -> msg
handleDataTypeChange config =
    Maybe.andThen DataType.fromId
        >> Maybe.withDefault DataType.none
        >> config.handleDataTypeChange


dataTypeSelectChildren : Config msg -> DataType -> List (Html msg)
dataTypeSelectChildren config dataType =
    defaultOption :: selectOptions config dataType


selectOptions : Config msg -> DataType -> List (Html msg)
selectOptions config dataType =
    List.map (selectOption config dataType) DataType.all


defaultOption : Html msg
defaultOption =
    option [] [ text "Data Type" ]


selectOption : Config msg -> DataType -> DataType -> Html msg
selectOption config currentType dataType =
    option
        [ selected (currentType == dataType)
        , value (DataType.toId dataType |> toString)
        ]
        [ text (DataType.toStringValue dataType) ]


modifierView : Config msg -> DataType.Modifier -> Maybe (Html msg)
modifierView config modifier =
    case modifier of
        DataType.NoModifier ->
            Nothing

        DataType.Size size ->
            Just (sizeInput config.handleModifierChange size)

        DataType.Precision precision decimals ->
            Just (precisionInput config.handleModifierChange precision decimals)

        DataType.WithTimezone withTimezone ->
            Just (timezoneCheckbox config.handleModifierChange withTimezone)


sizeInput : (DataType.Modifier -> msg) -> Maybe Int -> Html msg
sizeInput handleModifierChange size =
    div []
        [ label []
            [ text "Size" ]
        , input
            [ type_ "number"
            , value (maybeIntToString size)
            , onIntInput (DataType.Size >> handleModifierChange)
            ]
            []
        ]


precisionInput : (DataType.Modifier -> msg) -> Maybe Int -> Maybe Int -> Html msg
precisionInput handleModifierChange precision decimals =
    div []
        [ div []
            [ label [] [ text "Precision" ]
            , input
                [ type_ "number"
                , onIntInput (flip DataType.Precision decimals >> handleModifierChange)
                , value (maybeIntToString precision)
                ]
                []
            ]
        , div []
            [ label [] [ text "Decimals" ]
            , input
                [ type_ "number"
                , onIntInput (DataType.Precision precision >> handleModifierChange)
                , value (maybeIntToString decimals)
                ]
                []
            ]
        ]


timezoneCheckbox : (DataType.Modifier -> msg) -> Bool -> Html msg
timezoneCheckbox handleModifierChange isChecked =
    label []
        [ text "With Timezone"
        , input
            [ type_ "checkbox"
            , checked isChecked
            , onChangeBool (DataType.WithTimezone >> handleModifierChange)
            ]
            []
        ]


maybeIntToString : Maybe Int -> String
maybeIntToString =
    Maybe.map toString >> Maybe.withDefault ""
