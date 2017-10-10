module Data.Entity
    exposing
        ( Entity
        , decoder
        , empty
        , encode
        , encodeNew
        , init
        , updateName
        )

import Json.Decode as JD exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as JE exposing (Value)


type alias Entity =
    { id : Int
    , name : String
    , schemaId : Int
    }


empty : Entity
empty =
    { id = 0
    , name = ""
    , schemaId = 0
    }


init : Int -> Entity
init schemaId =
    { empty | schemaId = schemaId }


updateName : String -> Entity -> Entity
updateName name entity =
    { entity | name = name }


decoder : Decoder Entity
decoder =
    decode Entity
        |> required "id" int
        |> required "name" string
        |> required "schemaId" int


encode : Entity -> Value
encode entity =
    JE.object
        [ ( "entity"
          , JE.object
                [ ( "name", JE.string entity.name )
                , ( "schema_id", JE.int entity.schemaId )
                ]
          )
        ]


encodeNew : Entity -> Value
encodeNew { name, schemaId } =
    JE.object
        [ ( "entity"
          , JE.object
                [ ( "name", JE.string name )
                , ( "schema_id", JE.int schemaId )
                ]
          )
        ]
