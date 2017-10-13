module Request.Table
    exposing
        ( create
        , destroy
        , index
        , one
        , oneWithAll
        , oneWithColumns
        , oneWithSchema
        , update
        )

import Data.Combined as Combined
    exposing
        ( TableWithAll
        , TableWithColumns
        , TableWithSchema
        )
import Data.Table as Table exposing (Table)
import Http exposing (Request)
import Json.Decode as JD
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


tablesUrl : String
tablesUrl =
    baseUrl ++ "tables/"


tableUrl : Int -> String
tableUrl =
    toString >> (++) tablesUrl



-- CREATE


create : Table -> Request Table
create table =
    Http.post tablesUrl
        (Table.encodeNew table |> Http.jsonBody)
        (dataDecoder Table.decoder)



-- READ


index : Request (List Table)
index =
    Http.get tablesUrl (dataDecoder (JD.list Table.decoder))


one : Int -> Request Table
one id =
    Http.get (tableUrl id) (dataDecoder Table.decoder)


oneWithSchema : Int -> Http.Request TableWithSchema
oneWithSchema id =
    Http.get
        (tableUrl id |> Combined.withSchema)
        (dataDecoder Combined.tableWithSchemaDecoder)


oneWithColumns : Int -> Http.Request TableWithColumns
oneWithColumns id =
    Http.get
        (tableUrl id |> Combined.withColumns)
        (dataDecoder Combined.tableWithColumnsDecoder)


oneWithAll : Int -> Http.Request TableWithAll
oneWithAll id =
    Http.get
        (tableUrl id |> Combined.withSchema |> Combined.andWithColumns)
        (dataDecoder Combined.tableWithAllDecoder)



-- UPDATE


update : Table -> Request Table
update table =
    put
        (tableUrl table.id)
        (Table.encode table |> Http.jsonBody)
        (dataDecoder Table.decoder)



-- DELETE


destroy : Int -> Request ()
destroy id =
    delete (tableUrl id)
