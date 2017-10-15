module Views.DataType.Select exposing (view)

import Data.DataType as DataType exposing (DataType(..))
import Html as Html exposing (Attribute, Html, div, input, label, option, select, text)
import Html.Attributes exposing (checked, name, placeholder, selected, type_, value)
import Utils.Handlers exposing (onChangeBool, onChangeInt, onIntInput)


view : (DataType -> msg) -> DataType -> Html msg
view handleChange dataType =
    div [] (children handleChange dataType)


children : (DataType -> msg) -> DataType -> List (Html msg)
children handleChange dataType =
    modifierView handleChange dataType
        |> Maybe.map
            (List.singleton >> (::) (dataTypeSelect handleChange dataType))
        |> Maybe.withDefault [ dataTypeSelect handleChange dataType ]


dataTypeSelect : (DataType -> msg) -> DataType -> Html msg
dataTypeSelect handleChange dataType =
    select
        [ onChangeInt (mapDataTypeChange handleChange) ]
        (dataTypeSelectChildren handleChange dataType)


mapDataTypeChange : (DataType -> msg) -> Maybe Int -> msg
mapDataTypeChange handleChange =
    Maybe.andThen DataType.fromId
        >> Maybe.withDefault DataType.none
        >> handleChange


dataTypeSelectChildren : (DataType -> msg) -> DataType -> List (Html msg)
dataTypeSelectChildren handleChange dataType =
    defaultOption :: selectOptions handleChange dataType


selectOptions : (DataType -> msg) -> DataType -> List (Html msg)
selectOptions handleChange dataType =
    List.map (selectOption handleChange dataType) DataType.all


defaultOption : Html msg
defaultOption =
    option [] [ text "Data Type" ]


selectOption : (DataType -> msg) -> DataType -> DataType -> Html msg
selectOption handleChange currentType dataType =
    option
        [ selected (DataType.isSame dataType currentType)
        , value (DataType.toId dataType |> toString)
        ]
        [ text (DataType.toShortName dataType) ]


modifierView : (DataType -> msg) -> DataType -> Maybe (Html msg)
modifierView handleChange dataType =
    case dataType of
        Char size ->
            Just <| sizeInput Char handleChange size

        VarChar size ->
            Just <| sizeInput VarChar handleChange size

        Bit size ->
            Just <| sizeInput Bit handleChange size

        VarBit size ->
            Just <| sizeInput VarBit handleChange size

        Numeric precision scale ->
            Just <| precisionScaleInput Numeric handleChange precision scale

        TimeStamp withTimezone ->
            Just <| timezoneCheckbox TimeStamp handleChange withTimezone

        Time withTimezone ->
            Just <| timezoneCheckbox Time handleChange withTimezone

        otherType ->
            Nothing


sizeInput : (Maybe Int -> DataType) -> (DataType -> msg) -> Maybe Int -> Html msg
sizeInput toDataType handleChange size =
    div []
        [ label []
            [ text "Size" ]
        , input
            [ type_ "number"
            , placeholder (toString DataType.defaultSize)
            , value (maybeIntToString size)
            , onIntInput (toDataType >> handleChange)
            ]
            []
        ]


precisionScaleInput :
    (Maybe Int -> Maybe Int -> DataType)
    -> (DataType -> msg)
    -> Maybe Int
    -> Maybe Int
    -> Html msg
precisionScaleInput toDataType handleDataTypeChange precision scale =
    div []
        [ div []
            [ label [] [ text "Precision" ]
            , input
                [ type_ "number"
                , placeholder (toString DataType.defaultPrecision)
                , onIntInput (flip toDataType scale >> handleDataTypeChange)
                , value (maybeIntToString precision)
                ]
                []
            ]
        , div []
            [ label [] [ text "Scale" ]
            , input
                [ type_ "number"
                , placeholder (toString DataType.defaultScale)
                , onIntInput (toDataType precision >> handleDataTypeChange)
                , value (maybeIntToString scale)
                ]
                []
            ]
        ]


timezoneCheckbox : (Bool -> DataType) -> (DataType -> msg) -> Bool -> Html msg
timezoneCheckbox toDataType handleDataTypeChange isChecked =
    label []
        [ text "With Timezone"
        , input
            [ type_ "checkbox"
            , checked isChecked
            , onChangeBool (toDataType >> handleDataTypeChange)
            ]
            []
        ]


maybeIntToString : Maybe Int -> String
maybeIntToString =
    Maybe.map toString >> Maybe.withDefault ""
