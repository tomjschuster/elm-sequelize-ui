module Request.Column
    exposing
        ( create
        , destroy
        , forTable
        , one
        , oneWithAll
        , oneWithTable
        , update
        , updateWithConstraints
        )

import Data.Column as Column exposing (Column)
import Data.Combined as Combined
    exposing
        ( ColumnWithAll
        , ColumnWithConstraints
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


tableColumnsUrl : Int -> String
tableColumnsUrl tableId =
    baseUrl ++ "tables/" ++ toString tableId ++ "/columns"


create : Column -> Request ColumnWithConstraints
create column =
    Http.post
        columnsUrl
        (Column.encodeNew column |> Http.jsonBody)
        (dataDecoder <| Combined.columnWithConstraintsDecoder)


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


forTable : Int -> Request (List Column)
forTable tableId =
    Http.get
        (tableColumnsUrl tableId)
        (dataDecoder (JD.list Column.decoder))


update : Column -> Request Column
update column =
    put (columnUrl column.id)
        (Column.encode column |> Http.jsonBody)
        (dataDecoder Column.decoder)


updateWithConstraints : Column -> Request ColumnWithConstraints
updateWithConstraints column =
    put (columnUrl column.id)
        (Column.encode column |> Http.jsonBody)
        (dataDecoder Combined.columnWithConstraintsDecoder)


destroy : Int -> Request ()
destroy id =
    delete (columnUrl id)
