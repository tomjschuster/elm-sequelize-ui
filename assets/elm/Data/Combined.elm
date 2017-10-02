module Data.Combined
    exposing
        ( EntityWithAll
        , EntityWithFields
        , EntityWithSchema
        , FieldWithAll
        , FieldWithEntity
        , SchemaWithEntities
        , entityWithAllDecoder
        , entityWithFieldsDecoder
        , entityWithSchemaDecoder
        , fieldWithAllDecoder
        , fieldWithEntityDecoder
        , schemaWithEntitiesDecoder
        )

import Data.Entity as Entity exposing (Entity)
import Data.Field as Field exposing (Field)
import Data.Schema as Schema exposing (Schema)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias SchemaWithEntities =
    { schema : Schema, entities : List Entity }


schemaWithEntitiesDecoder : Decoder SchemaWithEntities
schemaWithEntitiesDecoder =
    decode SchemaWithEntities
        |> required "schema" Schema.decoder
        |> required "entities" (JD.list Entity.decoder)


type alias EntityWithSchema =
    { entity : Entity, schema : Schema }


entityWithSchemaDecoder : Decoder EntityWithSchema
entityWithSchemaDecoder =
    decode EntityWithSchema
        |> required "entity" Entity.decoder
        |> required "schema" Schema.decoder


type alias EntityWithFields =
    { entity : Entity, fields : List Field }


entityWithFieldsDecoder : Decoder EntityWithFields
entityWithFieldsDecoder =
    decode EntityWithFields
        |> required "entity" Entity.decoder
        |> required "fields" (JD.list Field.decoder)


type alias EntityWithAll =
    { entity : Entity, schema : Schema, fields : List Field }


entityWithAllDecoder : Decoder EntityWithAll
entityWithAllDecoder =
    decode EntityWithAll
        |> required "entity" Entity.decoder
        |> required "schema" Schema.decoder
        |> required "fields" (JD.list Field.decoder)


type alias FieldWithEntity =
    { field : Field, entity : Entity }


fieldWithEntityDecoder : Decoder FieldWithEntity
fieldWithEntityDecoder =
    decode FieldWithEntity
        |> required "field" Field.decoder
        |> required "entity" Entity.decoder


type alias FieldWithAll =
    { field : Field, entity : Entity, schema : Schema }


fieldWithAllDecoder : Decoder FieldWithAll
fieldWithAllDecoder =
    decode FieldWithAll
        |> required "field" Field.decoder
        |> required "entity" Entity.decoder
        |> required "schema" Schema.decoder
