module Views.DataType.Display exposing (view)

import Data.DataType as DataType exposing (DataType)
import Html exposing (Html, span, text)


view : DataType -> Html msg
view dataType =
    span [] [ text (DataType.toLongName dataType) ]
