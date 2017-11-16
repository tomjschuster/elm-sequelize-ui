module Views.Column.Edit exposing (view)

import Data.Column as Column exposing (EditingColumn)
import Html exposing (Attribute, Html, div, text)
import Views.Column.ConstraintFields as ConstraintFields
import Views.Column.ReferenceFields as ReferenceFields
import Views.Columns.DataTypeSelect as DataTypesSelect


view : EditingColumn -> Html msg
view editingColumn =
    text ""
