module Views.Column.Edit exposing (view)

import Data.Column as Column exposing (Column)
import Data.Column.Constraints as ColumnConstraints
import Data.Table exposing (Table)
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
import Views.Column.ConstraintFields as ConstraintFields
import Views.Column.DataTypeSelect as DataTypesSelect
import Views.Column.ReferenceFields as ReferenceFields


view : msg -> (Column -> msg) -> String -> Column -> List Table -> List Column -> Html msg
view saveMsg toUpdateMsg saveText column tables columns =
    form
        []
        [ fieldset []
            [ legend [] [ text "Create a column" ]
            , p
                []
                [ newColumnInput
                    (flip Column.updateName column >> toUpdateMsg)
                    column.name
                ]
            , p
                []
                [ DataTypesSelect.view
                    "create-column-data-type"
                    (flip Column.updateDataType column >> toUpdateMsg)
                    column.dataType
                ]
            , ConstraintFields.view
                "create-column-constraints"
                (flip Column.updateConstraints column >> toUpdateMsg)
                column.constraints
            , ReferenceFields.view
                (flip ColumnConstraints.updateReferences column.constraints
                    >> flip Column.updateConstraints column
                    >> toUpdateMsg
                )
                column.constraints.references
                tables
                columns
            , saveButton saveMsg saveText
            ]
        ]


newColumnInput : (String -> msg) -> String -> Html msg
newColumnInput toNameMsg name =
    label []
        [ text "Name"
        , input
            [ id "create-column"
            , value name
            , onInput toNameMsg
            ]
            []
        ]


saveButton : msg -> String -> Html msg
saveButton saveMsg saveText =
    button [ type_ "button", onClick saveMsg ] [ text saveText ]
