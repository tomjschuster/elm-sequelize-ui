module Utils.Html exposing (nodeList)

import Html exposing (Html)


nodeList : List ( Html msg, Bool ) -> List (Html msg)
nodeList =
    List.foldr
        (\( node, isDisplayed ) acc ->
            if isDisplayed then
                node :: acc
            else
                acc
        )
        []
