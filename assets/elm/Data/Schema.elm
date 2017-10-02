module Data.Schema
    exposing
        ( Schema
        , decoder
        , empty
        , encode
        , encodeNewSchema
        )

import Json.Decode exposing (Decoder, Value, int, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as JE


type alias Schema =
    { id : Int
    , name : String
    }


empty : Schema
empty =
    Schema 0 ""


decoder : Decoder Schema
decoder =
    decode Schema
        |> required "id" int
        |> required "name" string


encode : Schema -> JE.Value
encode schema =
    JE.object
        [ ( "schema"
          , JE.object
                [ ( "id", JE.int schema.id )
                , ( "name", JE.string schema.name )
                ]
          )
        ]


encodeNewSchema : String -> JE.Value
encodeNewSchema name =
    JE.object
        [ ( "schema", JE.object [ ( "name", JE.string name ) ] ) ]
