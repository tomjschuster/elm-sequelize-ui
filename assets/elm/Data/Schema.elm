module Data.Schema exposing (Schema, decoder, empty, encode)

import Data.Entity as Entity exposing (Entity)
import Json.Decode exposing (Decoder, Value, int, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as JE


type alias Schema =
    { id : Int
    , name : String
    , entities : List Entity
    }


empty : Schema
empty =
    Schema 0 "" []


decoder : Decoder Schema
decoder =
    decode Schema
        |> required "id" int
        |> required "name" string
        |> optional "entities" (list Entity.decoder) []


encode : Schema -> JE.Value
encode schema =
    JE.object
        [ ( "id", JE.int schema.id )
        , ( "name", JE.string schema.name )
        ]
