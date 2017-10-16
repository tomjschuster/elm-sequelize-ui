module Views.DataType.Select exposing (view)

import Data.DataType as DataType exposing (DataType(..))
import Html as Html exposing (Attribute, Html, div, input, label, optgroup, option, select, span, text)
import Html.Attributes exposing (attribute, checked, for, id, name, placeholder, selected, type_, value)
import Utils.Handlers exposing (onChangeBool, onChangeInt, onIntInput)


view : String -> (DataType -> msg) -> DataType -> Html msg
view viewId handleChange dataType =
    label [ for viewId ] (text "Data Type" :: children viewId handleChange dataType)


children : String -> (DataType -> msg) -> DataType -> List (Html msg)
children viewId handleChange dataType =
    case modifierView viewId handleChange dataType of
        Just modifier ->
            [ dtSelect viewId handleChange dataType, modifier ]

        Nothing ->
            [ dtSelect viewId handleChange dataType ]


dtSelect : String -> (DataType -> msg) -> DataType -> Html msg
dtSelect viewId handleChange dataType =
    select
        [ id viewId
        , onChangeInt (mapDataTypeChange handleChange)
        ]
        (dtSelectChildren handleChange dataType)


mapDataTypeChange : (DataType -> msg) -> Maybe Int -> msg
mapDataTypeChange handleChange =
    Maybe.andThen DataType.fromId
        >> Maybe.withDefault DataType.none
        >> handleChange


dtSelectChildren : (DataType -> msg) -> DataType -> List (Html msg)
dtSelectChildren handleChange dataType =
    defaultOption :: dtOptGroups handleChange dataType


dtOptGroups : (DataType -> msg) -> DataType -> List (Html msg)
dtOptGroups handleChange dataType =
    [ dtOptGroup handleChange "Character Types" DataType.characterGroup dataType
    , dtOptGroup handleChange "Numeric Types" DataType.numericGroup dataType
    , dtOptGroup handleChange "Boolean Types" DataType.booleanGroup dataType
    , dtOptGroup handleChange "Bit String Types" DataType.bitStringGroup dataType
    , dtOptGroup handleChange "Monetary Types" DataType.monetaryGroup dataType
    , dtOptGroup handleChange "Date/Time Types" DataType.dateTimeGroup dataType
    ]


dtOptGroup : (DataType -> msg) -> String -> List DataType -> DataType -> Html msg
dtOptGroup handleChange name dataTypes dataType =
    optgroup
        [ attribute "label" name ]
        (List.map (dtOption handleChange dataType) dataTypes)


dtOptions : String -> (DataType -> msg) -> DataType -> List (Html msg)
dtOptions viewId handleChange dataType =
    List.map (dtOption handleChange dataType) DataType.all


defaultOption : Html msg
defaultOption =
    option [] [ text "-" ]


dtOption : (DataType -> msg) -> DataType -> DataType -> Html msg
dtOption handleChange currentType dataType =
    option
        [ selected (DataType.isSame dataType currentType)
        , value (DataType.toId dataType |> toString)
        ]
        [ text (DataType.toShortName dataType) ]


modifierView : String -> (DataType -> msg) -> DataType -> Maybe (Html msg)
modifierView viewId handleChange dataType =
    case dataType of
        Char size ->
            Just <| sizeInput Char viewId handleChange size

        VarChar size ->
            Just <| sizeInput VarChar viewId handleChange size

        Bit size ->
            Just <| sizeInput Bit viewId handleChange size

        VarBit size ->
            Just <| sizeInput VarBit viewId handleChange size

        Decimal precision scale ->
            Just <| precisionScaleInput Decimal viewId handleChange precision scale

        TimeStamp withTimezone ->
            Just <| timezoneCheckbox TimeStamp viewId handleChange withTimezone

        Time withTimezone ->
            Just <| timezoneCheckbox Time viewId handleChange withTimezone

        otherType ->
            Nothing


sizeInput : (Maybe Int -> DataType) -> String -> (DataType -> msg) -> Maybe Int -> Html msg
sizeInput toDataType viewId handleChange size =
    label []
        [ text "Size"
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
    -> String
    -> (DataType -> msg)
    -> Maybe Int
    -> Maybe Int
    -> Html msg
precisionScaleInput toDataType viewId handleDataTypeChange precision scale =
    span []
        [ precisionInput toDataType viewId handleDataTypeChange precision scale
        , scaleInput toDataType viewId handleDataTypeChange precision scale
        ]


precisionInput :
    (Maybe Int -> Maybe Int -> DataType)
    -> String
    -> (DataType -> msg)
    -> Maybe Int
    -> Maybe Int
    -> Html msg
precisionInput toDataType viewId handleDataTypeChange precision scale =
    label
        [ for (viewId ++ "-precision") ]
        [ text "Precision"
        , input
            [ id (viewId ++ "-precision")
            , type_ "number"
            , placeholder (toString DataType.defaultPrecision)
            , onIntInput (flip toDataType scale >> handleDataTypeChange)
            , value (maybeIntToString precision)
            ]
            []
        ]


scaleInput :
    (Maybe Int -> Maybe Int -> DataType)
    -> String
    -> (DataType -> msg)
    -> Maybe Int
    -> Maybe Int
    -> Html msg
scaleInput toDataType viewId handleDataTypeChange precision scale =
    label
        [ for (viewId ++ "-scale") ]
        [ text "Scale"
        , input
            [ id (viewId ++ "-scale")
            , type_ "number"
            , placeholder (toString DataType.defaultScale)
            , onIntInput (toDataType precision >> handleDataTypeChange)
            , value (maybeIntToString scale)
            ]
            []
        ]


timezoneCheckbox : (Bool -> DataType) -> String -> (DataType -> msg) -> Bool -> Html msg
timezoneCheckbox toDataType viewId handleDataTypeChange isChecked =
    label
        [ for (viewId ++ "-timezone") ]
        [ text "With Timezone"
        , input
            [ id (viewId ++ "-timezone")
            , type_ "checkbox"
            , checked isChecked
            , onChangeBool (toDataType >> handleDataTypeChange)
            ]
            []
        ]


maybeIntToString : Maybe Int -> String
maybeIntToString =
    Maybe.map toString >> Maybe.withDefault ""
