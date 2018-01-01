module Data.Column.Constraints
    exposing
        ( ColumnConstraints
        , addConstraint
        , default
        , dictFromList
        , encode
        , fromList
        , fromTableConstraints
        , updateDefaultValue
        , updateHasDefaultValue
        , updateIsNotNull
        , updateIsPrimaryKey
        , updateIsUnique
        , updateOrEraseDefaultValue
        , updateReference
        )

import Data.Column exposing (Column)
import Data.Column.Reference as Reference exposing (Reference)
import Data.Constraint as Constraint exposing (Constraint)
import Data.Table.Constraints as TableConstraints exposing (TableConstraints)
import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)
import Utils.List as ListUtils


type alias ColumnConstraints =
    { isPrimaryKey : Bool
    , isNotNull : Bool
    , defaultValue : Maybe String
    , isUnique : Bool
    , reference : Reference
    }


default : ColumnConstraints
default =
    { isPrimaryKey = False
    , isNotNull = False
    , defaultValue = Nothing
    , isUnique = False
    , reference = Reference.None
    }



--


fromList :
    Dict Int Column
    -> Int
    -> List Constraint
    -> ColumnConstraints
fromList columnLookup columnId =
    List.foldr
        (\constraint acc ->
            if Constraint.columnId constraint == Just columnId then
                addConstraint columnLookup constraint acc
            else
                acc
        )
        default


dictFromList :
    Dict Int Column
    -> List Constraint
    -> Dict Int ColumnConstraints
dictFromList columnLookup =
    List.foldr
        (\constraint acc ->
            constraint
                |> Constraint.columnId
                |> Maybe.map
                    (\columnId ->
                        Dict.update columnId
                            (Maybe.withDefault default
                                >> addConstraint columnLookup constraint
                                >> Just
                            )
                            acc
                    )
                |> Maybe.withDefault acc
        )
        Dict.empty



-- UPDATE


addConstraint : Dict Int Column -> Constraint -> ColumnConstraints -> ColumnConstraints
addConstraint columnLookup constraint constraints =
    case constraint of
        Constraint.PK _ ->
            updateIsPrimaryKey True constraints

        Constraint.NN _ ->
            updateIsNotNull True constraints

        Constraint.DV (Constraint.DefaultValue _ _ _ defaultValue) ->
            updateDefaultValue defaultValue constraints

        Constraint.UQ _ ->
            updateIsUnique True constraints

        Constraint.FK fk ->
            fk
                |> Constraint.singleReference
                |> Maybe.map
                    (\columnId ->
                        columnLookup
                            |> Dict.get columnId
                            |> Maybe.map (.tableId >> flip Reference.Complete columnId)
                            |> Maybe.withDefault Reference.None
                            |> flip updateReference constraints
                    )
                |> Maybe.withDefault constraints


fromTableConstraints :
    Dict Int Column
    -> Int
    -> TableConstraints
    -> ColumnConstraints
fromTableConstraints columnLookup columnId tableConstraints =
    { isPrimaryKey = TableConstraints.isPrimaryKey columnId tableConstraints
    , isNotNull = TableConstraints.isNotNull columnId tableConstraints
    , defaultValue = TableConstraints.defaultValue columnId tableConstraints
    , isUnique = TableConstraints.isUnique columnId tableConstraints
    , reference =
        tableConstraints.foreignKeys
            |> ListUtils.find (Constraint.inSingleForeignKey columnId)
            |> Maybe.andThen Constraint.singleReference
            |> Maybe.andThen (Reference.fromColumnId columnLookup)
            |> Maybe.withDefault Reference.None
    }



-- UPDATE


updateIsPrimaryKey : Bool -> ColumnConstraints -> ColumnConstraints
updateIsPrimaryKey isPrimaryKey constraints =
    { constraints | isPrimaryKey = isPrimaryKey }


updateIsNotNull : Bool -> ColumnConstraints -> ColumnConstraints
updateIsNotNull isNotNull constraints =
    { constraints | isNotNull = isNotNull }


updateHasDefaultValue : Bool -> ColumnConstraints -> ColumnConstraints
updateHasDefaultValue hasDefaultValue constraints =
    if hasDefaultValue then
        { constraints | defaultValue = Just "" }
    else
        { constraints | defaultValue = Nothing }


updateDefaultValue : String -> ColumnConstraints -> ColumnConstraints
updateDefaultValue defaultValue constraints =
    { constraints | defaultValue = Just defaultValue }


updateOrEraseDefaultValue : Maybe String -> ColumnConstraints -> ColumnConstraints
updateOrEraseDefaultValue defaultValue constraints =
    { constraints | defaultValue = defaultValue }


updateIsUnique : Bool -> ColumnConstraints -> ColumnConstraints
updateIsUnique isUnique constraints =
    { constraints | isUnique = isUnique }


updateReference : Reference -> ColumnConstraints -> ColumnConstraints
updateReference reference constraints =
    { constraints | reference = reference }



-- ENCODE


encode : ColumnConstraints -> Value
encode { isPrimaryKey, isNotNull, defaultValue, isUnique, reference } =
    JE.object
        [ ( "is_primary_key", JE.bool isPrimaryKey )
        , ( "is_not_null", JE.bool isNotNull )
        , ( "default_value"
          , defaultValue |> Maybe.map JE.string |> Maybe.withDefault JE.null
          )
        , ( "is_unique", JE.bool isUnique )
        , ( "reference"
          , Reference.encode reference
          )
        ]
