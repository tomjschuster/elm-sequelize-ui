module Data.Column
    exposing
        ( Column
        , decoder
        , empty
        , encode
        , encodeNew
        , init
        , removeFromList
        , replaceIfMatch
        , updateDataType
        , updateName
        )

import Data.DataType as DataType exposing (DataType)
import Json.Decode as JD exposing (Decoder, int, maybe, string)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)
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
    }


empty : Column
empty =
    { id = 0
    , tableId = 0
    , name = ""
    , dataType = DataType.none
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
    { column | dataType = dataType }


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
        |> custom DataType.decoder


encode : Column -> Value
encode { id, tableId, name, dataType } =
    JE.object
        [ ( "column"
          , JE.object
                ([ ( "id", JE.int id )
                 , ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 ]
                    ++ DataType.encode dataType
                )
          )
        ]


encodeNew : Column -> Value
encodeNew { tableId, name, dataType } =
    JE.object
        [ ( "column"
          , JE.object
                ([ ( "table_id", JE.int tableId )
                 , ( "name", JE.string name )
                 ]
                    ++ DataType.encode dataType
                )
          )
        ]