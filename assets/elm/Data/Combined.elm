module Data.Combined
    exposing
        ( FieldWithAll
        , FieldWithTable
        , SchemaWithTables
        , TableWithAll
        , TableWithFields
        , TableWithSchema
        , fieldWithAllDecoder
        , fieldWithTableDecoder
        , schemaWithTablesDecoder
        , tableWithAllDecoder
        , tableWithFieldsDecoder
        , tableWithSchemaDecoder
        )

import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


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
