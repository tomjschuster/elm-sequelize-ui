module Data.Field exposing (Field, decoder, empty, encode, encodeNewField)

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
        |> required "entityId" int


encode : Field -> Value
encode { id, name, entityId } =
    JE.object
        [ ( "field"
          , JE.object
                [ ( "id", JE.int id )
                , ( "name", JE.string name )
                , ( "entity_id", JE.int entityId )
                ]
          )
        ]


encodeNewField : String -> Int -> Value
encodeNewField name entityId =
    JE.object
        [ ( "field"
          , JE.object
                [ ( "name", JE.string name )
                , ( "entity_id", JE.int entityId )
                ]
          )
        ]
