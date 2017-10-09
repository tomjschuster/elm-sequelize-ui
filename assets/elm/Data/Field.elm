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
import Json.Decode as JD exposing (Decoder, int, maybe, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)


{-

   Constraint (Primary Key, Not Null, Default, Unique, Auto-increment)
   Comment VARCHAR(255)
-}
{-

   Constraint (Primary Key, Not Null, Default, Unique, Auto-increment)
   Comment VARCHAR(255)
-}


type alias Field =
    { id : Int
    , entityId : Int
    , name : String
    , dataType : DataType
    , dataTypeModifier : DataType.Modifier
    }


empty : Field
empty =
    { id = 0
    , entityId = 0
    , name = ""
    , dataType = DataType.none
    , dataTypeModifier = DataType.NoModifier
    }


updateDataType : Field -> DataType -> Field
updateDataType field dataType =
    { field | dataType = dataType }


decoder : Decoder Field
decoder =
    decode Field
        |> required "id" int
        |> required "entityId" int
        |> required "name" string
        |> optional "dataTypeId" DataType.decoder DataType.none
        |> hardcoded DataType.NoModifier


encode : Field -> Value
encode { id, entityId, name, dataType } =
    JE.object
        [ ( "field"
          , JE.object
                [ ( "id", JE.int id )
                , ( "entity_id", JE.int entityId )
                , ( "name", JE.string name )
                , ( "data_type_id", DataType.encode dataType )
                ]
          )
        ]


encodeNewField : Int -> String -> DataType -> DataType.Modifier -> Value
encodeNewField entityId name dataType modifier =
    JE.object
        [ ( "field"
          , JE.object
                ([ ( "entity_id", JE.int entityId )
                 , ( "name", JE.string name )
                 , ( "data_type_id", DataType.encode dataType )
                 ]
                    ++ DataType.encodeModifier modifier
                )
          )
        ]
