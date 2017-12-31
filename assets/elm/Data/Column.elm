module Data.Column
    exposing
        ( Column
        , decoder
        , empty
        , encode
        , init
        , removeFromList
        , updateDataType
        , updateName
        )

import Data.DataType as DataType exposing (DataType)
import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, required)
import Json.Encode as JE exposing (Value)


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
        ([ ( "id", JE.int id )
         , ( "table_id", JE.int tableId )
         , ( "name", JE.string name )
         ]
            ++ DataType.encode dataType
        )
