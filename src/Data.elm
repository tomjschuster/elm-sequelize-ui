module Data exposing (Schema, emptySchema, encodeSchema, schemaDecoder)

import Json.Decode exposing (Decoder, Value, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE


type alias Schema =
    { id : Int
    , name : String
    }


emptySchema : Schema
emptySchema =
    Schema 0 ""


schemaDecoder : Decoder Schema
schemaDecoder =
    decode Schema
        |> required "id" int
        |> required "name" string


encodeSchema : Schema -> JE.Value
encodeSchema schema =
    JE.object
        [ ( "id", JE.int schema.id )
        , ( "name", JE.string schema.name )
        ]
