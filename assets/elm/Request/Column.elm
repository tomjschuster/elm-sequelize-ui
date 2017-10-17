module Request.Column exposing (create, destroy, one, oneWithAll, oneWithTable, update)

import Data.Column as Column exposing (Column)
import Data.Combined as Combined
    exposing
        ( ColumnWithAll
        , ColumnWithTable
        )
import Http exposing (Request)
import Json.Decode as JD
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


columnsUrl : String
columnsUrl =
    baseUrl ++ "columns/"


columnUrl : Int -> String
columnUrl =
    toString >> (++) columnsUrl


create : Column -> Request Column
create column =
    Http.post
        columnsUrl
        (Column.encodeNew column |> Http.jsonBody)
        (dataDecoder <| JD.field "column" Column.decoder)


one : Int -> Request Column
one id =
    Http.get (columnUrl id) (dataDecoder Column.decoder)


oneWithTable : Int -> Request ColumnWithTable
oneWithTable id =
    Http.get
        (columnUrl id |> Combined.withTable)
        (dataDecoder Combined.columnWithTableDecoder)


oneWithAll : Int -> Request ColumnWithAll
oneWithAll id =
    Http.get
        (columnUrl id |> Combined.withTable |> Combined.andWithSchema)
        (dataDecoder Combined.columnWithAllDecoder)


update : Column -> Request Column
update column =
    put (columnUrl column.id)
        (Column.encode column |> Http.jsonBody)
        (dataDecoder Column.decoder)


destroy : Int -> Request ()
destroy id =
    delete (columnUrl id)
