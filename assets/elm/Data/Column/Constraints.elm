module Data.Column.Constraints
    exposing
        ( ColumnConstraints
        , default
        , encode
        , updateDefaultValue
        , updateHasDefaultValue
        , updateIsNotNull
        , updateIsPrimaryKey
        , updateIsUnique
        , updateReferences
        )

import Data.Column.Reference as Reference exposing (Reference)
import Data.Constraint as Constraint
    exposing
        ( DefaultValue
        , ForeignKey
        , NotNull
        , PrimaryKey
        , UniqueKey
        )
import Data.Table.Constraints exposing (TableConstraints)
import Json.Encode as JE exposing (Value)


type alias ColumnConstraints =
    { isPrimaryKey : Bool
    , isNotNull : Bool
    , defaultValue : Maybe String
    , isUnique : Bool
    , references : List Reference
    }


default : ColumnConstraints
default =
    { isPrimaryKey = False
    , isNotNull = False
    , defaultValue = Nothing
    , isUnique = False
    , references = []
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


updateIsUnique : Bool -> ColumnConstraints -> ColumnConstraints
updateIsUnique isUnique constraints =
    { constraints | isUnique = isUnique }


updateReferences : List Reference -> ColumnConstraints -> ColumnConstraints
updateReferences references constraints =
    { constraints | references = references }



-- HELPERS


isPrimaryKey : Int -> TableConstraints -> Bool
isPrimaryKey columnId =
    .primaryKey
        >> Maybe.map (Constraint.inPrimaryKey columnId)
        >> Maybe.withDefault False


isNotNull : Int -> TableConstraints -> Bool
isNotNull columnId =
    .notNulls
        >> List.filter (Constraint.isNotNull columnId)
        >> List.isEmpty
        >> not


defaultValue : Int -> TableConstraints -> Maybe String
defaultValue columnId =
    .defaultValues
        >> List.filterMap (Constraint.defaultValue columnId)
        >> List.head


isUnique : Int -> TableConstraints -> Bool
isUnique columnId =
    .uniqueKeys
        >> List.filter (Constraint.isUnique columnId)
        >> List.isEmpty
        >> not


singleReferences : Int -> TableConstraints -> List Int
singleReferences columnId =
    .foreignKeys
        >> List.filter (Constraint.inSingleForeignKey columnId)
        >> List.filterMap Constraint.singleReference



-- ENCODE


encode : ColumnConstraints -> Value
encode { isPrimaryKey, isNotNull, defaultValue, isUnique, references } =
    JE.object
        [ ( "is_primary_key", JE.bool isPrimaryKey )
        , ( "is_not_null", JE.bool isNotNull )
        , ( "default_value"
          , defaultValue |> Maybe.map JE.string |> Maybe.withDefault JE.null
          )
        , ( "is_unique", JE.bool isUnique )
        , ( "references", Reference.encode references )
        ]
