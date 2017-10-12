module Request.Table
    exposing
        ( all
        , create
        , destroy
        , one
        , oneWithAll
        , oneWithFields
        , oneWithSchema
        , update
        )

import Data.Combined as Combined exposing (TableWithAll, TableWithFields, TableWithSchema)
import Data.Table as Table exposing (Table)
import Http exposing (Request)
import Json.Decode as JD
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


tablesUrl : String
tablesUrl =
    baseUrl ++ "tables/"


withFields : String -> String
withFields =
    flip (++) "?" >> flip (++) "&fields=show"


withSchema : String -> String
withSchema =
    flip (++) "?" >> flip (++) "&schema=show"


withAll : String -> String
withAll =
    flip (++) "?" >> flip (++) "&schema=show" >> flip (++) "&fields=show"


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


all : Request (List Table)
all =
    Http.get tablesUrl (dataDecoder (JD.list Table.decoder))


one : Int -> Request Table
one id =
    Http.get (tableUrl id) (dataDecoder Table.decoder)


oneWithSchema : Int -> Http.Request TableWithSchema
oneWithSchema id =
    Http.get
        (tableUrl id |> withSchema)
        (dataDecoder Combined.tableWithSchemaDecoder)


oneWithFields : Int -> Http.Request TableWithFields
oneWithFields id =
    Http.get
        (tableUrl id |> withFields)
        (dataDecoder Combined.tableWithFieldsDecoder)


oneWithAll : Int -> Http.Request TableWithAll
oneWithAll id =
    Http.get
        (tableUrl id |> withAll)
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
