module Data.Entity exposing (Entity, encodeEntity, entityDecoder)

import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (Value)


type alias Entity =
    { id : Int
    , name : String
    , schemaId : Int
    }


entityDecoder : Decoder Entity
entityDecoder =
    decode Entity
        |> required "id" int
        |> required "name" string
        |> required "schemaId" int


encodeEntity : Entity -> Value
encodeEntity entity =
    JE.object
        [ ( "name", JE.string entity.name )
        , ( "schemaId", JE.int entity.schemaId )
        ]
