module Data.Combined
    exposing
        ( ColumnWithAll
        , ColumnWithConstraints
        , ColumnWithTable
        , DbEntity(..)
        , SchemaWithTables
        , TableWithAll
        , TableWithColumns
        , TableWithSchema
        , andWithColumns
        , andWithSchema
        , andWithTable
        , andWithTables
        , columnWithAllDecoder
        , columnWithConstraintsDecoder
        , columnWithTableDecoder
        , schemaWithTablesDecoder
        , tableWithAllDecoder
        , tableWithColumnsDecoder
        , tableWithSchemaDecoder
        , withColumns
        , withConstraints
        , withSchema
        , withTable
        , withTables
        )

import Data.Column as Column exposing (Column)
import Data.Constraints as Constraints exposing (Constraints)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


-- DB ENTITY


type DbEntity
    = DbSchema Schema
    | DbNewSchema Schema
    | DbUpdatedSchema Schema
    | DbSchemas (List Schema)
    | DbTable Table
    | DbNewTable Table
    | DbUpdatedTable Table
    | DbTables (List Table)
    | DbColumn Column
    | DbNewColumn Column
    | DbUpdatedColumn Column
    | DbColumns (List Column)
    | DbConstraints Constraints



-- COMBINE REQUESTS


with : String -> String -> String
with entity =
    flip (++) ("?with=" ++ entity)


withSchema : String -> String
withSchema =
    with "schema"


withTable : String -> String
withTable =
    with "table"


withTables : String -> String
withTables =
    with "tables"


withColumns : String -> String
withColumns =
    with "columns"


withConstraints : String -> String
withConstraints =
    with "constraints"


andWith : String -> String -> String
andWith entity =
    flip (++) ("," ++ entity)


andWithSchema : String -> String
andWithSchema =
    andWith "schema"


andWithTable : String -> String
andWithTable =
    andWith "table"


andWithTables : String -> String
andWithTables =
    andWith "tables"


andWithColumns : String -> String
andWithColumns =
    andWith "columns"



-- TYPES AND DECODERS


type alias SchemaWithTables =
    { schema : Schema, tables : List Table }


schemaWithTablesDecoder : Decoder SchemaWithTables
schemaWithTablesDecoder =
    decode SchemaWithTables
        |> required "schema" Schema.decoder
        |> required "tables" (JD.list Table.decoder)


type alias TableWithSchema =
    { table : Table, schema : Schema }


tableWithSchemaDecoder : Decoder TableWithSchema
tableWithSchemaDecoder =
    decode TableWithSchema
        |> required "table" Table.decoder
        |> required "schema" Schema.decoder


type alias TableWithColumns =
    { table : Table, columns : List Column }


tableWithColumnsDecoder : Decoder TableWithColumns
tableWithColumnsDecoder =
    decode TableWithColumns
        |> required "table" Table.decoder
        |> required "columns" (JD.list Column.decoder)


type alias TableWithAll =
    { table : Table, schema : Schema, columns : List Column, constraints : Constraints }


tableWithAllDecoder : Decoder TableWithAll
tableWithAllDecoder =
    decode TableWithAll
        |> required "table" Table.decoder
        |> required "schema" Schema.decoder
        |> required "columns" (JD.list Column.decoder)
        |> required "constraints" Constraints.decoder


type alias ColumnWithTable =
    { column : Column, table : Table }


columnWithTableDecoder : Decoder ColumnWithTable
columnWithTableDecoder =
    decode ColumnWithTable
        |> required "column" Column.decoder
        |> required "table" Table.decoder


type alias ColumnWithConstraints =
    { column : Column, constraints : Constraints }


columnWithConstraintsDecoder : Decoder ColumnWithConstraints
columnWithConstraintsDecoder =
    decode ColumnWithConstraints
        |> required "column" Column.decoder
        |> required "constraints" Constraints.decoder


type alias ColumnWithAll =
    { column : Column, table : Table, schema : Schema }


columnWithAllDecoder : Decoder ColumnWithAll
columnWithAllDecoder =
    decode ColumnWithAll
        |> required "column" Column.decoder
        |> required "table" Table.decoder
        |> required "schema" Schema.decoder
