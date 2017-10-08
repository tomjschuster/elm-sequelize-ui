module Data.Field
    exposing
        ( Field
        , decoder
        , empty
        , encode
        , encodeNewField
        , updateDataType
        )

import Data.DataType as DataType exposing (DataType)
import Json.Decode as JD exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import Json.Encode as JE exposing (Value)


{-

   DataType (Lookup)
   Constraint (Primary Key, Not Null, Default, Unique, Auto-increment)
   Comment VARCHAR(255)
-}
{-

   DataType (Lookup)
   Constraint (Primary Key, Not Null, Default, Unique, Auto-increment)
   Comment VARCHAR(255)
-}


type alias Field =
    { id : Int
    , entityId : Int
    , name : String
    , dataType : Maybe DataType
    }


empty : Field
empty =
    { id = 0
    , entityId = 0
    , name = ""
    , dataType = Nothing
    }


updateDataType : Field -> DataType -> Field
updateDataType field dataType =
    { field | dataType = Just dataType }


decoder : Decoder Field
decoder =
    decode Field
        |> required "id" int
        |> required "entityId" int
        |> required "name" string
        |> hardcoded Nothing


encode : Field -> Value
encode { id, name, entityId } =
    JE.object
        [ ( "field"
          , JE.object
                [ ( "id", JE.int id )
                , ( "entity_id", JE.int entityId )
                , ( "name", JE.string name )
                ]
          )
        ]


encodeNewField : String -> Int -> Value
encodeNewField name entityId =
    JE.object
        [ ( "field"
          , JE.object
                [ ( "entity_id", JE.int entityId )
                , ( "name", JE.string name )
                ]
          )
        ]
