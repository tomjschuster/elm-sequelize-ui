module Views.Column.DataTypeSelect exposing (view)

import Data.DataType as DataType exposing (DataType(..))
import Html exposing (Html, input, label, optgroup, option, select, span, text)
import Html.Attributes as Attr
import Utils.Events as EvtUtils


view : String -> (DataType -> msg) -> DataType -> Html msg
view viewId handleChange dataType =
    label [ Attr.for viewId ] (text "Data Type" :: children viewId handleChange dataType)


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
        [ Attr.id viewId
        , EvtUtils.onChangeInt (mapDataTypeChange handleChange)
        ]
        (dtSelectChildren dataType)


mapDataTypeChange : (DataType -> msg) -> Maybe Int -> msg
mapDataTypeChange handleChange =
    Maybe.andThen DataType.fromId
        >> Maybe.withDefault DataType.none
        >> handleChange


dtSelectChildren : DataType -> List (Html msg)
dtSelectChildren dataType =
    defaultOption :: dtOptGroups dataType


dtOptGroups : DataType -> List (Html msg)
dtOptGroups dataType =
    [ dtOptGroup "Character Types" DataType.characterGroup dataType
    , dtOptGroup "Numeric Types" DataType.numericGroup dataType
    , dtOptGroup "Boolean Types" DataType.booleanGroup dataType
    , dtOptGroup "Bit String Types" DataType.bitStringGroup dataType
    , dtOptGroup "Monetary Types" DataType.monetaryGroup dataType
    , dtOptGroup "Date/Time Types" DataType.dateTimeGroup dataType
    ]


dtOptGroup : String -> List DataType -> DataType -> Html msg
dtOptGroup name dataTypes dataType =
    optgroup
        [ Attr.attribute "label" name ]
        (List.map (dtOption dataType) dataTypes)


defaultOption : Html msg
defaultOption =
    option [] [ text "-" ]


dtOption : DataType -> DataType -> Html msg
dtOption currentType dataType =
    option
        [ Attr.selected (DataType.isSameType dataType currentType)
        , Attr.value (DataType.toId dataType |> toString)
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

        _ ->
            Nothing


sizeInput : (Maybe Int -> DataType) -> String -> (DataType -> msg) -> Maybe Int -> Html msg
sizeInput toDataType _ handleChange size =
    label []
        [ text "Size"
        , input
            [ Attr.type_ "number"
            , Attr.placeholder (toString DataType.defaultSize)
            , Attr.value (maybeIntToString size)
            , EvtUtils.onIntInput (toDataType >> handleChange)
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
        [ Attr.for (viewId ++ "-precision") ]
        [ text "Precision"
        , input
            [ Attr.id (viewId ++ "-precision")
            , Attr.type_ "number"
            , Attr.placeholder (toString DataType.defaultPrecision)
            , EvtUtils.onIntInput (flip toDataType scale >> handleDataTypeChange)
            , Attr.value (maybeIntToString precision)
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
        [ Attr.for (viewId ++ "-scale") ]
        [ text "Scale"
        , input
            [ Attr.id (viewId ++ "-scale")
            , Attr.type_ "number"
            , Attr.placeholder (toString DataType.defaultScale)
            , EvtUtils.onIntInput (toDataType precision >> handleDataTypeChange)
            , Attr.value (maybeIntToString scale)
            ]
            []
        ]


timezoneCheckbox : (Bool -> DataType) -> String -> (DataType -> msg) -> Bool -> Html msg
timezoneCheckbox toDataType viewId handleDataTypeChange isChecked =
    label
        [ Attr.for (viewId ++ "-timezone") ]
        [ text "With Timezone"
        , input
            [ Attr.id (viewId ++ "-timezone")
            , Attr.type_ "checkbox"
            , Attr.checked isChecked
            , EvtUtils.onChangeBool (toDataType >> handleDataTypeChange)
            ]
            []
        ]


maybeIntToString : Maybe Int -> String
maybeIntToString =
    Maybe.map toString >> Maybe.withDefault ""
