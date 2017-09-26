module Data.Schema exposing (Schema, emptySchema, encodeSchema, schemaDecoder)

import Data.Entity exposing (Entity, encodeEntity, entityDecoder)
import Json.Decode exposing (Decoder, Value, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE


type alias Schema =
    { id : Int
    , name : String
    , entities : List Entity
    }


emptySchema : Schema
emptySchema =
    Schema 0 "" []


schemaDecoder : Decoder Schema
schemaDecoder =
    decode Schema
        |> required "id" int
        |> required "name" string
        |> required "entities" (list entityDecoder)


encodeSchema : Schema -> JE.Value
encodeSchema schema =
    JE.object
        [ ( "id", JE.int schema.id )
        , ( "name", JE.string schema.name )
        ]
