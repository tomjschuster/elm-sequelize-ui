module Data.Combined
    exposing
        ( FieldWithAll
        , FieldWithTable
        , SchemaWithTables
        , TableWithAll
        , TableWithFields
        , TableWithSchema
        , andWithFields
        , andWithSchema
        , andWithTable
        , andWithTables
        , fieldWithAllDecoder
        , fieldWithTableDecoder
        , schemaWithTablesDecoder
        , tableWithAllDecoder
        , tableWithFieldsDecoder
        , tableWithSchemaDecoder
        , withFields
        , withSchema
        , withTable
        , withTables
        )

import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


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


withFields : String -> String
withFields =
    with "fields"


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


andWithFields : String -> String
andWithFields =
    andWith "fields"



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


type alias TableWithFields =
    { table : Table, fields : List Field }


tableWithFieldsDecoder : Decoder TableWithFields
tableWithFieldsDecoder =
    decode TableWithFields
        |> required "table" Table.decoder
        |> required "fields" (JD.list Field.decoder)


type alias TableWithAll =
    { table : Table, schema : Schema, fields : List Field }


tableWithAllDecoder : Decoder TableWithAll
tableWithAllDecoder =
    decode TableWithAll
        |> required "table" Table.decoder
        |> required "schema" Schema.decoder
        |> required "fields" (JD.list Field.decoder)


type alias FieldWithTable =
    { field : Field, table : Table }


fieldWithTableDecoder : Decoder FieldWithTable
fieldWithTableDecoder =
    decode FieldWithTable
        |> required "field" Field.decoder
        |> required "table" Table.decoder


type alias FieldWithAll =
    { field : Field, table : Table, schema : Schema }


fieldWithAllDecoder : Decoder FieldWithAll
fieldWithAllDecoder =
    decode FieldWithAll
        |> required "field" Field.decoder
        |> required "table" Table.decoder
        |> required "schema" Schema.decoder
