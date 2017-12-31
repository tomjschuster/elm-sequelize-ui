module Views.Column.ConstraintsDisplay exposing (view)

import Data.Column exposing (Column)
import Data.Column.Constraints exposing (ColumnConstraints)
import Data.Column.Reference as Reference exposing (Reference)
import Data.Table exposing (Table)
import Dict exposing (Dict)
import Html exposing (Html, span, text)


type alias ConstraintDisplay =
    ( String, Bool )


view :
    Dict Int Table
    -> Dict Int Column
    -> ColumnConstraints
    -> Html msg
view tableLookup columnLookup constraints =
    span []
        [ constraintsTextNode tableLookup columnLookup constraints ]


constraintsTextNode :
    Dict Int Table
    -> Dict Int Column
    -> ColumnConstraints
    -> Html msg
constraintsTextNode tableLookup columnLookup =
    constraintDisplays tableLookup columnLookup >> getConstraintsText >> text


getConstraintsText : List ConstraintDisplay -> String
getConstraintsText =
    List.filter Tuple.second >> List.map Tuple.first >> String.join ", "


constraintDisplays :
    Dict Int Table
    -> Dict Int Column
    -> ColumnConstraints
    -> List ConstraintDisplay
constraintDisplays tableLookup columnLookup constraints =
    [ ( "primary key", constraints.isPrimaryKey )
    , ( "not null", constraints.isNotNull )
    , ( defaultText constraints.defaultValue, constraints.defaultValue /= Nothing )
    , ( "unique", constraints.isUnique )
    , ( displayReference tableLookup columnLookup constraints.reference
      , Reference.isComplete constraints.reference
      )
    ]


displayReference :
    Dict Int Table
    -> Dict Int Column
    -> Reference
    -> String
displayReference tableLookup columnLookup reference =
    case reference of
        Reference.Complete tableId columnId ->
            Maybe.map2
                (\table column ->
                    Reference.display table.name column.name
                )
                (Dict.get tableId tableLookup)
                (Dict.get columnId columnLookup)
                |> Maybe.withDefault ""

        _ ->
            ""


defaultText : Maybe String -> String
defaultText =
    Maybe.map ((++) "default: ") >> Maybe.withDefault ""
