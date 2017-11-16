module Views.Column.DataTypeDisplay exposing (view)

import Data.Column.DataType as DataType exposing (DataType)
import Html exposing (Html, span, text)


view : DataType -> Html msg
view dataType =
    span [] [ text (DataType.toLongName dataType) ]
