module Views.Column.Edit exposing (view)

import Data.Column as Column exposing (Column)
import Data.Column.Constraints as ColumnConstraints
import Data.Table exposing (Table)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , fieldset
        , form
        , input
        , label
        , legend
        , p
        , text
        )
import Html.Attributes exposing (id, type_, value)
import Html.Events exposing (onClick, onInput)
import Utils.List as ListUtils
import Views.Column.ConstraintFields as ConstraintFields
import Views.Column.DataTypeSelect as DataTypesSelect
import Views.Column.ReferenceSelect as ReferenceSelect


view : msg -> (Column -> msg) -> String -> Column -> List Table -> List Column -> Html msg
view saveMsg toUpdateMsg saveText column tables columns =
    let
        validColumnGroups =
            columns
                |> List.filter (.dataType >> (==) column.dataType)
                |> ListUtils.groupBy .tableId

        validTables =
            tables
                |> List.filter (.id >> flip Dict.member validColumnGroups)

        validColumns =
            validColumnGroups |> Dict.values |> List.concat

        maybeReferenceField =
            ReferenceSelect.view
                (flip ColumnConstraints.updateForeignKey column.constraints
                    >> flip Column.updateConstraints column
                    >> toUpdateMsg
                )
                column.constraints.reference
                validTables
                validColumns
    in
    case maybeReferenceField of
        Just referenceField ->
            form
                []
                [ fieldset []
                    [ nameField toUpdateMsg column
                    , dataTypeField toUpdateMsg column
                    , constraintFields toUpdateMsg column
                    , referenceField
                    , saveButton saveMsg saveText
                    ]
                ]

        Nothing ->
            form
                []
                [ fieldset []
                    [ nameField toUpdateMsg column
                    , dataTypeField toUpdateMsg column
                    , constraintFields toUpdateMsg column
                    , saveButton saveMsg saveText
                    ]
                ]



-- Name


nameField : (Column -> msg) -> Column -> Html msg
nameField toUpdateMsg column =
    p
        []
        [ nameInput
            (flip Column.updateName column >> toUpdateMsg)
            column.name
        ]


nameInput : (String -> msg) -> String -> Html msg
nameInput toNameMsg name =
    label []
        [ text "Name"
        , input
            [ id "create-column"
            , value name
            , onInput toNameMsg
            ]
            []
        ]



-- Data Type


dataTypeField : (Column -> msg) -> Column -> Html msg
dataTypeField toUpdateMsg column =
    p
        []
        [ DataTypesSelect.view
            "create-column-data-type"
            (flip Column.updateDataType column >> toUpdateMsg)
            column.dataType
        ]



-- Constraint Fields


constraintFields : (Column -> msg) -> Column -> Html msg
constraintFields toUpdateMsg column =
    ConstraintFields.view
        "create-column-constraints"
        (flip Column.updateConstraints column >> toUpdateMsg)
        column.constraints



-- Save Button


saveButton : msg -> String -> Html msg
saveButton saveMsg saveText =
    button [ type_ "button", onClick saveMsg ] [ text saveText ]
