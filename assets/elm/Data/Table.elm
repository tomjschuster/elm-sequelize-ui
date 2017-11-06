module Data.Table
    exposing
        ( Table
        , TableConstraints
        , buildConstraints
        , constraintsDecoder
        , decoder
        , empty
        , encode
        , encodeNew
        , init
        , replaceIfMatch
        , updateName
        )

import Data.Constraint as Constraint
    exposing
        ( Constraint
        , DefaultValue
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


type alias TableConstraints =
    { primaryKey : Maybe PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


defaultConstraints : TableConstraints
defaultConstraints =
    { primaryKey = Nothing
    , notNulls = []
    , defaultValues = []
    , uniqueKeys = []
    , foreignKeys = []
    }



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


buildConstraints : List Constraint -> TableConstraints
buildConstraints constraints =
    List.foldr updateConstraints defaultConstraints constraints


updateConstraints : Constraint -> TableConstraints -> TableConstraints
updateConstraints constraint tableConstraints =
    case constraint of
        Constraint.PK primaryKey ->
            { tableConstraints | primaryKey = Just primaryKey }

        Constraint.NN notNull ->
            { tableConstraints | notNulls = notNull :: tableConstraints.notNulls }

        Constraint.DV defaultValue ->
            { tableConstraints | defaultValues = defaultValue :: tableConstraints.defaultValues }

        Constraint.UQ uniqueKey ->
            { tableConstraints | uniqueKeys = uniqueKey :: tableConstraints.uniqueKeys }

        Constraint.FK foreignKey ->
            { tableConstraints | foreignKeys = foreignKey :: tableConstraints.foreignKeys }



-- DECODE/ENCODE


decoder : Decoder Table
decoder =
    decode Table
        |> required "id" JD.int
        |> required "name" JD.string
        |> required "schemaId" JD.int


constraintsDecoder : Decoder TableConstraints
constraintsDecoder =
    JD.list Constraint.decoder |> JD.map buildConstraints


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
