module Data.Column
    exposing
        ( Column
        , decoder
        , empty
        , encode
        , encodeNew
        , encodeNewColumn
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


type alias Column =
    { id : Int
    , tableId : Int
    , name : String
    , dataType : DataType
    , dataTypeModifier : DataType.Modifier
    }


empty : Column
empty =
    { id = 0
    , tableId = 0
    , name = ""
    , dataType = DataType.none
    , dataTypeModifier = DataType.NoModifier
    }


init : Int -> Column
init tableId =
    { empty | tableId = tableId }



-- UPDATE


updateName : String -> Column -> Column
updateName name column =
    { column | name = name }


updateDataType : DataType -> Column -> Column
updateDataType dataType column =
    { column
        | dataType = dataType
        , dataTypeModifier = DataType.toInitialModifier dataType
    }


updateDataTypeModifier : DataType.Modifier -> Column -> Column
updateDataTypeModifier dataTypeModifier column =
    { column | dataTypeModifier = dataTypeModifier }


updateSize : Maybe Int -> Column -> Column
updateSize size column =
    { column
        | dataTypeModifier =
            DataType.updateSize size column.dataTypeModifier
    }


updatePrecision : Maybe Int -> Maybe Int -> Column -> Column
updatePrecision precision decimals column =
    { column
        | dataTypeModifier =
            DataType.updatePrecision precision decimals column.dataTypeModifier
    }


updateWithTimezone : Bool -> Column -> Column
updateWithTimezone withTimezone column =
    { column
        | dataTypeModifier =
            DataType.updateWithTimezone withTimezone column.dataTypeModifier
    }


replaceIfMatch : Column -> Column -> Column
replaceIfMatch newColumn column =
    if column.id == newColumn.id then
        newColumn
    else
        column


removeFromList : List Column -> Int -> List Column
removeFromList columns id =
    List.filter (.id >> (/=) id) columns



-- DECODE/ENCODE


decoder : Decoder Column
decoder =
    decode Column
        |> required "id" int
        |> required "tableId" int
        |> required "name" string
        |> optional "dataTypeId" DataType.decoder DataType.none
        |> optional "modifier" DataType.modifierDecoder DataType.noModifier


encode : Column -> Value
encode { id, tableId, name, dataType, dataTypeModifier } =
    JE.object
        [ ( "column"
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


encodeNew : Column -> Value
encodeNew { tableId, name, dataType, dataTypeModifier } =
    JE.object
        [ ( "column"
          , JE.object
                ([ ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 , ( "data_type_id", DataType.encode dataType )
                 ]
                    ++ DataType.encodeModifier dataTypeModifier
                )
          )
        ]


encodeNewColumn : Int -> String -> DataType -> DataType.Modifier -> Value
encodeNewColumn tableId name dataType modifier =
    JE.object
        [ ( "column"
          , JE.object
                ([ ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 , ( "data_type_id", DataType.encode dataType )
                 ]
                    ++ DataType.encodeModifier modifier
                )
          )
        ]
