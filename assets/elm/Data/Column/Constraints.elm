module Data.Column.Constraints
    exposing
        ( ColumnConstraints
        , default
        , encode
        , updateDefaultValue
        , updateForeignKey
        , updateHasDefaultValue
        , updateIsNotNull
        , updateIsPrimaryKey
        , updateIsUnique
        )

import Data.Column.Reference as Reference exposing (Reference)
import Json.Encode as JE exposing (Value)


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
    , reference = Reference.SelectTable
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


updateForeignKey : Reference -> ColumnConstraints -> ColumnConstraints
updateForeignKey reference constraints =
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
