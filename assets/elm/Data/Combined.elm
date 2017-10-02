module Data.Combined
    exposing
        ( EntityWithAll
        , EntityWithSchema
        , FieldWithAll
        , FieldWithEntity
        , SchemaWithEntities
        , entityWithAllDecoder
        , entityWithSchemaDecoder
        , fieldWithAllDecoder
        , fieldWithEntityDecoder
        , schemaWithEntitiesDecoder
        )

import Entity exposing (Entity)
import Field exposing (Field)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Schema exposing (Schema)


type alias SchemaWithEntities =
    { schema : Schema, entities : List Entity }


schemaWithEntitiesDecoder : Decoder SchemaWithEntities
schemaWithEntitiesDecoder =
    decode SchemaWithEntities
        |> required "schema" Schema.decoder
        |> required "entities" (JD.list Entity.Decoder)


type alias EntityWithSchema =
    { entity : Entity, schema : Schema }


entityWithSchemaDecoder : Decoder EntityWithSchema
entityWithSchemaDecoder =
    decode EntityWithSchema
        |> required "entity" Entity.decoder
        |> required "schema" Schema.decoder


type alias EntityWithAll =
    { entity : Entity, schema : Schema, field : List Field }


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
