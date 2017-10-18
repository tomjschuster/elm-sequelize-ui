module Data.Table
    exposing
        ( Table
        , TableConstraints
        , decoder
        , empty
        , encode
        , encodeNew
        , init
        , replaceIfMatch
        , updateName
        )

import Data.Constraints as Constraints
    exposing
        ( DefaultValue
        , ForeignKey
        , NotNull
        , PrimaryKey
        , UniqueKey
        )
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as JE exposing (Value)


type alias Table =
    { id : Int
    , name : String
    , schemaId : Int
    , constraints : TableConstraints
    }


empty : Table
empty =
    { id = 0
    , name = ""
    , schemaId = 0
    , constraints = emptyConstraints
    }


type alias TableConstraints =
    { primaryKey : Maybe PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


emptyConstraints : TableConstraints
emptyConstraints =
    { primaryKey = Nothing
    , notNulls = []
    , defaultValues = []
    , uniqueKeys = []
    , foreignKeys = []
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
        |> required "constraints" constraintsDecoder


constraintsDecoder : Decoder TableConstraints
constraintsDecoder =
    decode TableConstraints
        |> required "primaryKey" (JD.maybe Constraints.primaryKeyDecoder)
        |> required "notNulls" (JD.list Constraints.notNullDecoder)
        |> required "defaultValues" (JD.list Constraints.defaultValueDecoder)
        |> required "uniqueKeys" (JD.list Constraints.uniqueKeyDecoder)
        |> required "foreignKeys" (JD.list Constraints.foreignKeyDecoder)


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
