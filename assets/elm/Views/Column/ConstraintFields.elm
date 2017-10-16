module Views.Column.ConstraintFields exposing (view)

import Data.Column as Column exposing (Column, ColumnConstraints)
import Html exposing (Attribute, Html, button, input, label, li, text, ul)
import Html.Attributes exposing (checked, for, id, type_, value)
import Html.Events exposing (onCheck, onInput)


view : String -> (Column -> msg) -> Column -> Html msg
view viewId toMsg column =
    ul
        []
        [ li [] [ primaryKeyCheckbox viewId toMsg column ]
        , li [] [ notNullCheckbox viewId toMsg column ]
        , li [] [ defaultView viewId toMsg column ]
        , li [] [ uniqueCheckbox viewId toMsg column ]
        ]


primaryKeyCheckbox : String -> (Column -> msg) -> Column -> Html msg
primaryKeyCheckbox viewId toMsg column =
    label
        [ for (viewId ++ "-primary-key") ]
        [ text "Primary Key"
        , input
            [ id (viewId ++ "-primary-key")
            , type_ "checkbox"
            , checked column.constraints.isPrimaryKey
            , onPrimaryKeyCheck toMsg column
            ]
            []
        ]


notNullCheckbox : String -> (Column -> msg) -> Column -> Html msg
notNullCheckbox viewId toMsg column =
    label
        [ for (viewId ++ "-not-null") ]
        [ text "Not Null"
        , input
            [ id (viewId ++ "-not-null")
            , type_ "checkbox"
            , checked column.constraints.isNotNull
            , onNotNullCheck toMsg column
            ]
            []
        ]


uniqueCheckbox : String -> (Column -> msg) -> Column -> Html msg
uniqueCheckbox viewId toMsg column =
    label
        [ for (viewId ++ "-unique") ]
        [ text "Unique"
        , input
            [ id (viewId ++ "-unique")
            , type_ "checkbox"
            , checked column.constraints.isUnique
            , onDefaultValueCheck toMsg column
            ]
            []
        ]


defaultView : String -> (Column -> msg) -> Column -> Html msg
defaultView viewId toMsg column =
    case column.constraints.defaultValue of
        Just value ->
            label
                [ for (viewId ++ "-default") ]
                [ text "Default Value"
                , defaultCheckBox viewId toMsg column
                , defaultInput viewId toMsg column
                ]

        Nothing ->
            label
                [ for (viewId ++ "-default") ]
                [ text "Default Value"
                , defaultCheckBox viewId toMsg column
                ]


defaultCheckBox : String -> (Column -> msg) -> Column -> Html msg
defaultCheckBox viewId toMsg column =
    input
        [ id (viewId ++ "-default")
        , type_ "checkbox"
        , checked (column.constraints.defaultValue /= Nothing)
        , onDefaultValueCheck toMsg column
        ]
        []


defaultInput : String -> (Column -> msg) -> Column -> Html msg
defaultInput viewId toMsg column =
    input
        [ id (viewId ++ "-default")
        , value (Maybe.withDefault "" column.constraints.defaultValue)
        , onDefaultValueInput toMsg column
        ]
        []



-- CUSTOM HANDLERS


onPrimaryKeyCheck : (Column -> msg) -> Column -> Attribute msg
onPrimaryKeyCheck toMsg column =
    onCheck (flip Column.updateIsPrimaryKey column >> toMsg)


onNotNullCheck : (Column -> msg) -> Column -> Attribute msg
onNotNullCheck toMsg column =
    onCheck (flip Column.updateIsNotNull column >> toMsg)


onDefaultValueCheck : (Column -> msg) -> Column -> Attribute msg
onDefaultValueCheck toMsg column =
    onCheck (flip Column.updateHasDefaultValue column >> toMsg)


onDefaultValueInput : (Column -> msg) -> Column -> Attribute msg
onDefaultValueInput toMsg column =
    onInput (flip Column.updateDefaultValue column >> toMsg)


onUniqueCheck : (Column -> msg) -> Column -> Attribute msg
onUniqueCheck toMsg column =
    onCheck (flip Column.updateIsUnique column >> toMsg)
