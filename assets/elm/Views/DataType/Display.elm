module Views.DataType.Display exposing (view)

import Data.DataType as DataType
import Html exposing (Html)


view : DataType.Modifier -> Html msg
view modifier =
    Html.div [] []
