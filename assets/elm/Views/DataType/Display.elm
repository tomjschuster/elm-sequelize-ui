module Views.DataType.Display exposing (view)

import Data.DataType as DataType exposing (DataType)
import Html exposing (Html, span, text)


view : DataType -> DataType.Modifier -> Html msg
view dataType modifier =
    span [] [ text (dataTypeString modifier dataType) ]


dataTypeString : DataType.Modifier -> DataType -> String
dataTypeString modifier =
    DataType.toStringValue >> flip (++) (modifierString modifier)


modifierString : DataType.Modifier -> String
modifierString =
    DataType.modifierToString >> Maybe.map ((++) " ") >> Maybe.withDefault ""
