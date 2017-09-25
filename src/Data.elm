module Data exposing (Schema, emptySchema, schemaDecoder)

import Json.Decode exposing (Decoder, Value, int, string)
import Json.Decode.Pipeline exposing (decode, required)


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
