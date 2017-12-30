module Data.Column
    exposing
        ( Column
        , buildConstraints
        , decoder
        , empty
        , encode
        , findAndAddConstraints
        , findAndAddEditingConstraints
        , init
        , removeFromList
        , replaceIfMatch
        , updateConstraints
        , updateDataType
        , updateName
        )

import Data.Column.Constraints as ColumnConstraints exposing (ColumnConstraints)
import Data.Column.DataType as DataType exposing (DataType)
import Data.Column.Reference as Reference exposing (Reference)
import Data.Constraint as Constraint
import Data.Table exposing (Table)
import Data.Table.Constraints exposing (TableConstraints)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, required)
import Json.Encode as JE exposing (Value)
import Utils.List as ListUtils


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
    , constraints = ColumnConstraints.default
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
    , reference =
        tableConstraints.foreignKeys
            |> ListUtils.find (Constraint.inSingleForeignKey columnId)
            |> Maybe.andThen Constraint.singleReference
            |> Maybe.andThen (referenceFromColumnId tableLookup columnLookup)
            |> Maybe.withDefault Reference.SelectTable
    }


buildEditingConstraints : Dict Int Column -> Int -> TableConstraints -> ColumnConstraints
buildEditingConstraints columnLookup columnId tableConstraints =
    { isPrimaryKey = isPrimaryKey columnId tableConstraints
    , isNotNull = isNotNull columnId tableConstraints
    , defaultValue = defaultValue columnId tableConstraints
    , isUnique = isUnique columnId tableConstraints
    , reference =
        tableConstraints.foreignKeys
            |> ListUtils.find (Constraint.inSingleForeignKey columnId)
            |> Maybe.andThen Constraint.singleReference
            |> Maybe.andThen (editingReferenceFromColumnId columnLookup)
            |> Maybe.withDefault Reference.SelectTable
    }


findAndAddConstraints : Dict Int Table -> Dict Int Column -> TableConstraints -> Column -> Column
findAndAddConstraints tableLookup columnLookup constraints column =
    { column | constraints = buildConstraints tableLookup columnLookup column.id constraints }


findAndAddEditingConstraints : Dict Int Column -> TableConstraints -> Column -> Column
findAndAddEditingConstraints columnLookup constraints column =
    { column | constraints = buildEditingConstraints columnLookup column.id constraints }



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


referenceFromColumnId : Dict Int Table -> Dict Int Column -> Int -> Maybe Reference
referenceFromColumnId tableLookup columnLookup columnId =
    let
        maybeColumn =
            Dict.get columnId columnLookup

        maybeTable =
            Maybe.andThen (.tableId >> flip Dict.get tableLookup) maybeColumn
    in
    Maybe.map2 (\c t -> Reference.Display t.id t.name c.id c.name) maybeColumn maybeTable


editingReferenceFromColumnId : Dict Int Column -> Int -> Maybe Reference
editingReferenceFromColumnId columnLookup columnId =
    columnLookup
        |> Dict.get columnId
        |> Maybe.map (.tableId >> flip Reference.Ready columnId)



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



-- DECODE/ENCODE


decoder : Decoder Column
decoder =
    decode Column
        |> required "id" int
        |> required "tableId" int
        |> required "name" string
        |> custom DataType.decoder
        |> hardcoded ColumnConstraints.default


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
        , ( "constraints", ColumnConstraints.encode constraints )
        ]
