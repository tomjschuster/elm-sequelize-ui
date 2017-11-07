module Request.Table
    exposing
        ( create
        , destroy
        , index
        , indexForSchema
        , indexForSchemaForDataType
        , indexReferences
        , one
        , tableUrl
        , update
        )

import Data.Table as Table exposing (Table)
import Http exposing (Request)
import Json.Decode as JD
import Request.Schema exposing (schemaUrl)
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


tablesUrl : String
tablesUrl =
    baseUrl ++ "tables/"


tableUrl : Int -> String
tableUrl =
    toString >> (++) tablesUrl


schemaTablesUrl : Int -> String
schemaTablesUrl =
    schemaUrl >> flip (++) "/tables"


schemaTablesForDataTypeUrl : Int -> Int -> String
schemaTablesForDataTypeUrl schemaId dataTypeId =
    schemaTablesUrl schemaId ++ "?data_type_id=" ++ toString dataTypeId


referencesUrl : Int -> String
referencesUrl =
    tableUrl >> flip (++) "/table-references"



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


indexForSchema : Int -> Http.Request (List Table)
indexForSchema schemaId =
    Http.get
        (schemaTablesUrl schemaId)
        (dataDecoder (JD.list Table.decoder))


indexReferences : Int -> Http.Request (List Table)
indexReferences tableId =
    Http.get
        (referencesUrl tableId)
        (dataDecoder (JD.list Table.decoder))


indexForSchemaForDataType : Int -> Int -> Http.Request (List Table)
indexForSchemaForDataType schemaId dataTypeId =
    Http.get
        (schemaTablesForDataTypeUrl schemaId dataTypeId)
        (dataDecoder (JD.list Table.decoder))



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
