module Views.Column.Edit exposing (view)

import Data.Column as Column exposing (Column)
import Data.Column.Constraints as ColumnConstraints exposing (ColumnConstraints)
import Data.DataType as DataType
import Data.Table exposing (Table)
import Dict
import Html
    exposing
        ( Html
        , button
        , fieldset
        , form
        , input
        , label
        , p
        , text
        )
import Html.Attributes as Attr
import Html.Events as Evt
import Utils.List as ListUtils
import Views.Column.ConstraintFields as ConstraintFields
import Views.Column.DataTypeSelect as DataTypesSelect
import Views.Column.ReferenceSelect as ReferenceSelect


view :
    msg
    -> (( Column, ColumnConstraints ) -> msg)
    -> String
    -> List Table
    -> List Column
    -> Column
    -> ColumnConstraints
    -> Html msg
view saveMsg toUpdateMsg saveText tables columns column constraints =
    let
        toColumnMsg =
            flip (,) constraints >> toUpdateMsg

        toConstraintsMsg =
            (,) column >> toUpdateMsg
    in
    form
        []
        [ fieldset []
            [ nameField toColumnMsg column
            , dataTypeField toColumnMsg column
            , constraintFields toConstraintsMsg constraints
            , referenceFields toConstraintsMsg column tables columns constraints
            , saveButton saveMsg saveText
            ]
        ]



-- Name


nameField :
    (Column -> msg)
    -> Column
    -> Html msg
nameField toColumnMsg column =
    p
        []
        [ nameInput
            (flip Column.updateName column >> toColumnMsg)
            column.name
        ]


nameInput : (String -> msg) -> String -> Html msg
nameInput toNameMsg name =
    label []
        [ text "Name"
        , input
            [ Attr.id "create-column"
            , Attr.value name
            , Evt.onInput toNameMsg
            ]
            []
        ]



-- Data Type


dataTypeField :
    (Column -> msg)
    -> Column
    -> Html msg
dataTypeField toColumnMsg column =
    p
        []
        [ DataTypesSelect.view
            "create-column-data-type"
            (flip Column.updateDataType column >> toColumnMsg)
            column.dataType
        ]



-- Constraint Fields


constraintFields :
    (ColumnConstraints -> msg)
    -> ColumnConstraints
    -> Html msg
constraintFields toConstraintsMsg constraints =
    ConstraintFields.view
        "create-column-constraints"
        toConstraintsMsg
        constraints



-- Reference Fields


referenceFields :
    (ColumnConstraints -> msg)
    -> Column
    -> List Table
    -> List Column
    -> ColumnConstraints
    -> Html msg
referenceFields toColumnMsg column tables columns constraints =
    let
        toReferenceMsg =
            flip ColumnConstraints.updateReference constraints
                >> toColumnMsg

        validColumnGroups =
            columns
                |> List.filter (.dataType >> DataType.isMatch column.dataType)
                |> ListUtils.groupBy .tableId

        validTables =
            tables
                |> List.filter (.id >> flip Dict.member validColumnGroups)

        validColumns =
            validColumnGroups |> Dict.values |> List.concat
    in
    p []
        [ text "References: "
        , ReferenceSelect.view
            toReferenceMsg
            validTables
            validColumns
            constraints.reference
        ]



-- Save Button


saveButton : msg -> String -> Html msg
saveButton saveMsg saveText =
    button [ Attr.type_ "button", Evt.onClick saveMsg ] [ text saveText ]
