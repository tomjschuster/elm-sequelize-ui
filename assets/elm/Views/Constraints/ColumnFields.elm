module Views.Constraints.ColumnFields exposing (view)

import Data.Constraints as Constraints exposing (ColumnConstraints)
import Html exposing (Attribute, Html, button, input, label, li, text, ul)
import Html.Attributes exposing (checked, for, id, type_, value)
import Html.Events exposing (onCheck, onInput)


view : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
view viewId toMsg constraints =
    ul
        []
        [ li [] [ primaryKeyCheckbox viewId toMsg constraints ]
        , li [] [ notNullCheckbox viewId toMsg constraints ]
        , li [] [ defaultView viewId toMsg constraints ]
        , li [] [ uniqueCheckbox viewId toMsg constraints ]
        ]


primaryKeyCheckbox : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
primaryKeyCheckbox viewId toMsg constraints =
    label
        [ for (viewId ++ "-primary-key") ]
        [ text "Primary Key"
        , input
            [ id (viewId ++ "-primary-key")
            , type_ "checkbox"
            , checked constraints.isPrimaryKey
            , onPrimaryKeyCheck toMsg constraints
            ]
            []
        ]


notNullCheckbox : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
notNullCheckbox viewId toMsg constraints =
    label
        [ for (viewId ++ "-not-null") ]
        [ text "Not Null"
        , input
            [ id (viewId ++ "-not-null")
            , type_ "checkbox"
            , checked constraints.isNotNull
            , onNotNullCheck toMsg constraints
            ]
            []
        ]


uniqueCheckbox : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
uniqueCheckbox viewId toMsg constraints =
    label
        [ for (viewId ++ "-unique") ]
        [ text "Unique"
        , input
            [ id (viewId ++ "-unique")
            , type_ "checkbox"
            , checked constraints.isUnique
            , onDefaultValueCheck toMsg constraints
            ]
            []
        ]


defaultView : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
defaultView viewId toMsg constraints =
    case constraints.defaultValue of
        Just value ->
            label
                [ for (viewId ++ "-default") ]
                [ text "Default Value"
                , defaultCheckBox viewId toMsg constraints
                , defaultInput viewId toMsg constraints
                ]

        Nothing ->
            label
                [ for (viewId ++ "-default") ]
                [ text "Default Value"
                , defaultCheckBox viewId toMsg constraints
                ]


defaultCheckBox : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
defaultCheckBox viewId toMsg constraints =
    input
        [ id (viewId ++ "-default")
        , type_ "checkbox"
        , checked (constraints.defaultValue /= Nothing)
        , onDefaultValueCheck toMsg constraints
        ]
        []


defaultInput : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
defaultInput viewId toMsg constraints =
    input
        [ id (viewId ++ "-default")
        , value (Maybe.withDefault "" constraints.defaultValue)
        , onDefaultValueInput toMsg constraints
        ]
        []



-- CUSTOM HANDLERS


onPrimaryKeyCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onPrimaryKeyCheck toMsg constraints =
    onCheck (flip Constraints.updateColumnIsPrimaryKey constraints >> toMsg)


onNotNullCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onNotNullCheck toMsg constraints =
    onCheck (flip Constraints.updateColumnIsNotNull constraints >> toMsg)


onDefaultValueCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onDefaultValueCheck toMsg constraints =
    onCheck (flip Constraints.updateColumnHasDefaultValue constraints >> toMsg)


onDefaultValueInput : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onDefaultValueInput toMsg constraints =
    onInput (flip Constraints.updateColumnDefaultValue constraints >> toMsg)


onUniqueCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onUniqueCheck toMsg constraints =
    onCheck (flip Constraints.updateColumnIsUnique constraints >> toMsg)
