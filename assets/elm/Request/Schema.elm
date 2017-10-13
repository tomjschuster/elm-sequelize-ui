module Request.Schema
    exposing
        ( create
        , destroy
        , index
        , one
        , oneWithTables
        , update
        )

import Data.Combined as Combined exposing (SchemaWithTables, withTables)
import Data.Schema as Schema exposing (Schema)
import Http
import Json.Decode as JD
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


schemasUrl : String
schemasUrl =
    baseUrl ++ "schemas/"


schemaUrl : Int -> String
schemaUrl =
    toString >> (++) schemasUrl


index : Http.Request (List Schema)
index =
    Http.get
        schemasUrl
        (dataDecoder (JD.list Schema.decoder))


oneWithTables : Int -> Http.Request SchemaWithTables
oneWithTables id =
    Http.get
        (schemaUrl id |> Combined.withTables)
        (dataDecoder Combined.schemaWithTablesDecoder)


one : Int -> Http.Request Schema
one id =
    Http.get (schemaUrl id) (dataDecoder Schema.decoder)


create : Schema -> Http.Request Schema
create schema =
    Http.post
        schemasUrl
        (Schema.encodeNew schema |> Http.jsonBody)
        (dataDecoder Schema.decoder)


update : Schema -> Http.Request Schema
update schema =
    put (schemaUrl schema.id)
        (Http.jsonBody (Schema.encode schema))
        (dataDecoder Schema.decoder)


destroy : Int -> Http.Request ()
destroy id =
    delete (schemaUrl id)
