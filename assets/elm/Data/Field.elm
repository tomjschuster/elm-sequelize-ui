module Data.Field exposing (Field, decoder, empty, encode)

import Json.Decode as JD exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (Value)


type alias Field =
    { id : Int
    , name : String
    , entityId : Int
    }


empty : Field
empty =
    Field 0 "" 0


decoder : Decoder Field
decoder =
    decode Field
        |> required "id" int
        |> required "name" string
        |> required "entity_id" int


encode : Field -> Value
encode { name, entityId } =
    JE.object
        [ ( "name", JE.string name )
        , ( "entity_id", JE.int entityId )
        ]
