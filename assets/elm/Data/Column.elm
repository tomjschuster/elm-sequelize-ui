module Data.Column
    exposing
        ( Column
        , ColumnConstraints
        , Reference
        , addReference
        , buildConstraints
        , decoder
        , empty
        , encode
        , encodeNew
        , findAndAddConstraints
        , findReferences
        , init
        , removeFromList
        , replaceIfMatch
        , updateConstraints
        , updateConstraintsDefaultValue
        , updateConstraintsHasDefaultValue
        , updateConstraintsIsNotNull
        , updateConstraintsIsPrimaryKey
        , updateConstraintsIsUnique
        , updateDataType
        , updateDefaultValue
        , updateHasDefaultValue
        , updateIsNotNull
        , updateIsPrimaryKey
        , updateIsUnique
        , updateName
        )

import Data.Constraint as Constraint
    exposing
        ( DefaultValue
        , ForeignKey
        , NotNull
        , PrimaryKey
        , UniqueKey
        )
import Data.DataType as DataType exposing (DataType)
import Data.Table as Table exposing (Table, TableConstraints)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, int, maybe, string)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)


{-

   Comment VARCHAR(255)
-}


type alias Column =
    { id : Int
    , tableId : Int
    , name : String
    , dataType : DataType
    , constraints : ColumnConstraints
    }


empty : Column
empty =
    { id = 0
    , tableId = 0
    , name = ""
    , dataType = DataType.none
    , constraints = defaultConstraints
    }


type alias ColumnConstraints =
    { isPrimaryKey : Bool
    , isNotNull : Bool
    , defaultValue : Maybe String
    , isUnique : Bool
    , references : List Reference
    }


type alias Reference =
    { columnId : Int
    , columnName : String
    , tableId : Int
    , tableName : String
    }


defaultConstraints : ColumnConstraints
defaultConstraints =
    { isPrimaryKey = False
    , isNotNull = False
    , defaultValue = Nothing
    , isUnique = False
    , references = []
    }


init : Int -> Column
init tableId =
    { empty | tableId = tableId }



-- UTILS


replaceIfMatch : Column -> Column -> Column
replaceIfMatch newColumn column =
    if column.id == newColumn.id then
        newColumn
    else
        column


buildConstraints : Dict Int Table -> Dict Int Column -> Int -> TableConstraints -> ColumnConstraints
buildConstraints tableLookup columnLookup columnId tableConstraints =
    { isPrimaryKey = isPrimaryKey columnId tableConstraints
    , isNotNull = isNotNull columnId tableConstraints
    , defaultValue = defaultValue columnId tableConstraints
    , isUnique = isUnique columnId tableConstraints
    , references =
        singleReferences columnId tableConstraints
            |> List.filterMap (lookupReferences tableLookup columnLookup)
    }


findAndAddConstraints : Dict Int Table -> Dict Int Column -> TableConstraints -> Column -> Column
findAndAddConstraints tableLookup columnLookup constraints column =
    { column | constraints = buildConstraints tableLookup columnLookup column.id constraints }



-- UPDATE


updateName : String -> Column -> Column
updateName name column =
    { column | name = name }


updateDataType : DataType -> Column -> Column
updateDataType dataType column =
    { column | dataType = dataType }


updateConstraints : ColumnConstraints -> Column -> Column
updateConstraints constraints column =
    { column | constraints = constraints }


removeFromList : List Column -> Int -> List Column
removeFromList columns id =
    List.filter (.id >> (/=) id) columns


updateIsPrimaryKey : Bool -> Column -> Column
updateIsPrimaryKey isPrimaryKey column =
    { column
        | constraints =
            updateConstraintsIsPrimaryKey isPrimaryKey column.constraints
    }


updateIsNotNull : Bool -> Column -> Column
updateIsNotNull isNotNull column =
    { column
        | constraints =
            updateConstraintsIsNotNull isNotNull column.constraints
    }


updateHasDefaultValue : Bool -> Column -> Column
updateHasDefaultValue hasDefaultValue column =
    { column
        | constraints =
            updateConstraintsHasDefaultValue hasDefaultValue column.constraints
    }


updateDefaultValue : String -> Column -> Column
updateDefaultValue defaultValue column =
    { column
        | constraints =
            updateConstraintsDefaultValue defaultValue column.constraints
    }


updateIsUnique : Bool -> Column -> Column
updateIsUnique isUnique column =
    { column
        | constraints =
            updateConstraintsIsUnique isUnique column.constraints
    }


addReference : Reference -> Column -> Column
addReference reference column =
    { column | constraints = addConstraintsReference reference column.constraints }



-- UPDATE CONSTRAINTS


updateConstraintsIsPrimaryKey : Bool -> ColumnConstraints -> ColumnConstraints
updateConstraintsIsPrimaryKey isPrimaryKey constraints =
    { constraints | isPrimaryKey = isPrimaryKey }


updateConstraintsIsNotNull : Bool -> ColumnConstraints -> ColumnConstraints
updateConstraintsIsNotNull isNotNull constraints =
    { constraints | isNotNull = isNotNull }


updateConstraintsHasDefaultValue : Bool -> ColumnConstraints -> ColumnConstraints
updateConstraintsHasDefaultValue hasDefaultValue constraints =
    if hasDefaultValue then
        { constraints | defaultValue = Just "" }
    else
        { constraints | defaultValue = Nothing }


updateConstraintsDefaultValue : String -> ColumnConstraints -> ColumnConstraints
updateConstraintsDefaultValue defaultValue constraints =
    { constraints | defaultValue = Just defaultValue }


updateConstraintsIsUnique : Bool -> ColumnConstraints -> ColumnConstraints
updateConstraintsIsUnique isUnique constraints =
    { constraints | isUnique = isUnique }


addConstraintsReference : Reference -> ColumnConstraints -> ColumnConstraints
addConstraintsReference reference constraints =
    { constraints | references = constraints.references ++ [ reference ] }



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


lookupReferences : Dict Int Table -> Dict Int Column -> Int -> Maybe Reference
lookupReferences tableLookup columnLookup columnId =
    let
        maybeColumn =
            Dict.get columnId columnLookup

        maybeTable =
            Maybe.andThen (.tableId >> flip Dict.get tableLookup) maybeColumn
    in
    Maybe.map2 (\c t -> Reference c.id c.name t.id t.name) maybeTable maybeColumn


findReferences : Int -> Int -> List Table -> List Column -> Maybe Reference
findReferences tableId columnId tables columns =
    let
        maybeColumn =
            List.filter (.id >> (==) columnId) columns |> List.head

        maybeTable =
            List.filter (.id >> (==) tableId) tables |> List.head
    in
    Maybe.map2 (\t c -> Reference c.id c.name t.id t.name) maybeTable maybeColumn



-- DECODE/ENCODE


decoder : Decoder Column
decoder =
    decode Column
        |> required "id" int
        |> required "tableId" int
        |> required "name" string
        |> custom DataType.decoder
        |> hardcoded defaultConstraints


encode : Column -> Value
encode { id, tableId, name, dataType, constraints } =
    JE.object
        [ ( "column"
          , JE.object
                ([ ( "id", JE.int id )
                 , ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 ]
                    ++ DataType.encode dataType
                )
          )
        , ( "constraints", encodeConstraints constraints )
        ]


encodeNew : Column -> Value
encodeNew { tableId, name, dataType, constraints } =
    JE.object
        [ ( "column"
          , JE.object
                ([ ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 ]
                    ++ DataType.encode dataType
                )
          )
        , ( "constraints", encodeConstraints constraints )
        ]


encodeConstraints : ColumnConstraints -> Value
encodeConstraints { isPrimaryKey, isNotNull, defaultValue, isUnique, references } =
    JE.object
        [ ( "is_primary_key", JE.bool isPrimaryKey )
        , ( "is_not_null", JE.bool isNotNull )
        , ( "default_value"
          , defaultValue |> Maybe.map JE.string |> Maybe.withDefault JE.null
          )
        , ( "is_unique", JE.bool isUnique )
        , ( "references", references |> List.map (.columnId >> JE.int) |> JE.list )
        ]
