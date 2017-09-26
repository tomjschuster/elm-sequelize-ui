module Data.Entity exposing (Entity, encodeEntity, entityDecoder)

import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (Value)


type alias Entity =
    { id : Int
    , name : String
    }


entityDecoder : Decoder Entity
entityDecoder =
    decode Entity
        |> required "id" int
        |> required "name" string


encodeEntity : Entity -> Value
encodeEntity entity =
    JE.object
        [ ( "id", JE.int entity.id )
        , ( "name", JE.string entity.name )
        ]
