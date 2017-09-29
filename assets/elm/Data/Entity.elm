module Data.Entity exposing (Entity, decoder, empty, encode)

import Data.Field as Field exposing (Field)
import Json.Decode as JD exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as JE exposing (Value)


type alias Entity =
    { id : Int
    , name : String
    , schemaId : Int
    , fields : List Field
    }


empty : Entity
empty =
    { id = 0
    , name = ""
    , schemaId = 0
    , fields = []
    }


decoder : Decoder Entity
decoder =
    decode Entity
        |> required "id" int
        |> required "name" string
        |> required "schemaId" int
        |> optional "fields" (JD.list Field.decoder) []


encode : Entity -> Value
encode entity =
    JE.object
        [ ( "name", JE.string entity.name )
        , ( "schemaId", JE.int entity.schemaId )
        ]
