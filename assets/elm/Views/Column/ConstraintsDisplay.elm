module Views.Column.ConstraintsDisplay exposing (view)

import Data.Column.Constraints as ColumnConstraints exposing (ColumnConstraints)
import Data.Column.Reference as Reference exposing (Reference)
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
    , ( Reference.listToString constraints.references, not (List.isEmpty constraints.references) )
    ]


defaultText : Maybe String -> String
defaultText =
    Maybe.map ((++) "default: ") >> Maybe.withDefault ""
