module Data.Table
    exposing
        ( Table
        , decoder
        , empty
        , encode
        , encodeNew
        , init
        , replaceIfMatch
        , updateName
        )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (Value)


type alias Table =
    { id : Int
    , name : String
    , schemaId : Int
    }


empty : Table
empty =
    { id = 0
    , name = ""
    , schemaId = 0
    }


init : Int -> Table
init schemaId =
    { empty | schemaId = schemaId }



-- UPDATE


updateName : String -> Table -> Table
updateName name table =
    { table | name = name }


replaceIfMatch : Table -> Table -> Table
replaceIfMatch newTable table =
    if table.id == newTable.id then
        newTable
    else
        table



-- DECODE/ENCODE


decoder : Decoder Table
decoder =
    decode Table
        |> required "id" JD.int
        |> required "name" JD.string
        |> required "schemaId" JD.int


encode : Table -> Value
encode table =
    JE.object
        [ ( "table"
          , JE.object
                [ ( "name", JE.string table.name )
                , ( "schema_id", JE.int table.schemaId )
                ]
          )
        ]


encodeNew : Table -> Value
encodeNew { name, schemaId } =
    JE.object
        [ ( "table"
          , JE.object
                [ ( "name", JE.string name )
                , ( "schema_id", JE.int schemaId )
                ]
          )
        ]
