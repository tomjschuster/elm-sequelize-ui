module Data.Field
    exposing
        ( Field
        , decoder
        , empty
        , encode
        , encodeNew
        , encodeNewField
        , init
        , removeFromList
        , replaceIfMatch
        , updateDataType
        , updateDataTypeModifier
        , updateName
        , updatePrecision
        , updateSize
        , updateWithTimezone
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
    , tableId : Int
    , name : String
    , dataType : DataType
    , dataTypeModifier : DataType.Modifier
    }


empty : Field
empty =
    { id = 0
    , tableId = 0
    , name = ""
    , dataType = DataType.none
    , dataTypeModifier = DataType.NoModifier
    }


init : Int -> Field
init tableId =
    { empty | tableId = tableId }



-- UPDATE


updateName : String -> Field -> Field
updateName name field =
    { field | name = name }


updateDataType : DataType -> Field -> Field
updateDataType dataType field =
    { field
        | dataType = dataType
        , dataTypeModifier = DataType.toInitialModifier dataType
    }


updateDataTypeModifier : DataType.Modifier -> Field -> Field
updateDataTypeModifier dataTypeModifier field =
    { field | dataTypeModifier = dataTypeModifier }


updateSize : Maybe Int -> Field -> Field
updateSize size field =
    { field
        | dataTypeModifier =
            DataType.updateSize size field.dataTypeModifier
    }


updatePrecision : Maybe Int -> Maybe Int -> Field -> Field
updatePrecision precision decimals field =
    { field
        | dataTypeModifier =
            DataType.updatePrecision precision decimals field.dataTypeModifier
    }


updateWithTimezone : Bool -> Field -> Field
updateWithTimezone withTimezone field =
    { field
        | dataTypeModifier =
            DataType.updateWithTimezone withTimezone field.dataTypeModifier
    }


replaceIfMatch : Field -> Field -> Field
replaceIfMatch newField field =
    if field.id == newField.id then
        newField
    else
        field


removeFromList : List Field -> Int -> List Field
removeFromList fields id =
    List.filter (.id >> (/=) id) fields



-- DECODE/ENCODE


decoder : Decoder Field
decoder =
    decode Field
        |> required "id" int
        |> required "tableId" int
        |> required "name" string
        |> optional "dataTypeId" DataType.decoder DataType.none
        |> optional "modifier" DataType.modifierDecoder DataType.noModifier


encode : Field -> Value
encode { id, tableId, name, dataType, dataTypeModifier } =
    JE.object
        [ ( "field"
          , JE.object
                ([ ( "id", JE.int id )
                 , ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 , ( "data_type_id", DataType.encode dataType )
                 ]
                    ++ DataType.encodeModifier dataTypeModifier
                )
          )
        ]


encodeNew : Field -> Value
encodeNew { tableId, name, dataType, dataTypeModifier } =
    JE.object
        [ ( "field"
          , JE.object
                ([ ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 , ( "data_type_id", DataType.encode dataType )
                 ]
                    ++ DataType.encodeModifier dataTypeModifier
                )
          )
        ]


encodeNewField : Int -> String -> DataType -> DataType.Modifier -> Value
encodeNewField tableId name dataType modifier =
    JE.object
        [ ( "field"
          , JE.object
                ([ ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 , ( "data_type_id", DataType.encode dataType )
                 ]
                    ++ DataType.encodeModifier modifier
                )
          )
        ]
