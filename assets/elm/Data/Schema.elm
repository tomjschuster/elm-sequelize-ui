module Data.Schema
    exposing
        ( Schema
        , decoder
        , empty
        , encode
        , encodeNew
        , updateName
        )

import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE


type alias Schema =
    { id : Int
    , name : String
    }


updateName : String -> Schema -> Schema
updateName name schema =
    { schema | name = name }


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


encodeNew : Schema -> JE.Value
encodeNew schema =
    JE.object
        [ ( "schema"
          , JE.object
                [ ( "name", JE.string schema.name )
                ]
          )
        ]
