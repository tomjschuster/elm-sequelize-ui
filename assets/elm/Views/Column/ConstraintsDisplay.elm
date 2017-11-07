module Views.Column.ConstraintsDisplay exposing (view)

import Data.Column as Column exposing (Column, ColumnConstraints, Reference)
import Html exposing (Html, span, text)


type alias ConstraintDisplay =
    ( String, Bool )


view : ColumnConstraints -> Html msg
view constraints =
    span [] [ constraintsTextNode constraints ]


constraintsTextNode : ColumnConstraints -> Html msg
constraintsTextNode =
    constraintDisplays >> getConstraintsText >> text


getConstraintsText : List ConstraintDisplay -> String
getConstraintsText =
    List.filter Tuple.second >> List.map Tuple.first >> String.join ", "


constraintDisplays : ColumnConstraints -> List ConstraintDisplay
constraintDisplays constraints =
    [ ( "primary key", constraints.isPrimaryKey )
    , ( "not null", constraints.isNotNull )
    , ( defaultText constraints.defaultValue, constraints.defaultValue /= Nothing )
    , ( "unique", constraints.isUnique )
    , ( referencesText constraints.references, not (List.isEmpty constraints.references) )
    ]


defaultText : Maybe String -> String
defaultText =
    Maybe.map ((++) "default: ") >> Maybe.withDefault ""


referencesText : List Reference -> String
referencesText references =
    "references: " ++ (List.map referenceText references |> String.join ", ")


referenceText : Reference -> String
referenceText { columnName, tableName } =
    tableName ++ "(" ++ columnName ++ ")"
