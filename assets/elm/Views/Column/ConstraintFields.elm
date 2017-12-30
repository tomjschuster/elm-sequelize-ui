module Views.Column.ConstraintFields exposing (view)

import Data.Column.Constraints as ColumnConstraints exposing (ColumnConstraints)
import Html exposing (Attribute, Html, input, label, li, text, ul)
import Html.Attributes as Attr
import Html.Events as Evt


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
        [ Attr.for (viewId ++ "-primary-key") ]
        [ text "Primary Key"
        , input
            [ Attr.id (viewId ++ "-primary-key")
            , Attr.type_ "checkbox"
            , Attr.checked constraints.isPrimaryKey
            , onPrimaryKeyCheck toMsg constraints
            ]
            []
        ]


notNullCheckbox : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
notNullCheckbox viewId toMsg constraints =
    label
        [ Attr.for (viewId ++ "-not-null") ]
        [ text "Not Null"
        , input
            [ Attr.id (viewId ++ "-not-null")
            , Attr.type_ "checkbox"
            , Attr.checked constraints.isNotNull
            , onNotNullCheck toMsg constraints
            ]
            []
        ]


uniqueCheckbox : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
uniqueCheckbox viewId toMsg constraints =
    label
        [ Attr.for (viewId ++ "-unique") ]
        [ text "Unique"
        , input
            [ Attr.id (viewId ++ "-unique")
            , Attr.type_ "checkbox"
            , Attr.checked constraints.isUnique
            , onUniqueCheck toMsg constraints
            ]
            []
        ]


defaultView : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
defaultView viewId toMsg constraints =
    case constraints.defaultValue of
        Just _ ->
            label
                [ Attr.for (viewId ++ "-default") ]
                [ text "Default Value"
                , defaultCheckBox viewId toMsg constraints
                , defaultInput viewId toMsg constraints
                ]

        Nothing ->
            label
                [ Attr.for (viewId ++ "-default") ]
                [ text "Default Value"
                , defaultCheckBox viewId toMsg constraints
                ]


defaultCheckBox : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
defaultCheckBox viewId toMsg constraints =
    input
        [ Attr.id (viewId ++ "-default")
        , Attr.type_ "checkbox"
        , Attr.checked (constraints.defaultValue /= Nothing)
        , onDefaultValueCheck toMsg constraints
        ]
        []


defaultInput : String -> (ColumnConstraints -> msg) -> ColumnConstraints -> Html msg
defaultInput viewId toMsg constraints =
    input
        [ Attr.id (viewId ++ "-default")
        , Attr.value (Maybe.withDefault "" constraints.defaultValue)
        , onDefaultValueInput toMsg constraints
        ]
        []



-- CUSTOM HANDLERS


onPrimaryKeyCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onPrimaryKeyCheck toMsg column =
    Evt.onCheck (flip ColumnConstraints.updateIsPrimaryKey column >> toMsg)


onNotNullCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onNotNullCheck toMsg column =
    Evt.onCheck (flip ColumnConstraints.updateIsNotNull column >> toMsg)


onDefaultValueCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onDefaultValueCheck toMsg column =
    Evt.onCheck (flip ColumnConstraints.updateHasDefaultValue column >> toMsg)


onDefaultValueInput : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onDefaultValueInput toMsg column =
    Evt.onInput (flip ColumnConstraints.updateDefaultValue column >> toMsg)


onUniqueCheck : (ColumnConstraints -> msg) -> ColumnConstraints -> Attribute msg
onUniqueCheck toMsg column =
    Evt.onCheck (flip ColumnConstraints.updateIsUnique column >> toMsg)
