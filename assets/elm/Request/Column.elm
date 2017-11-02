module Request.Column
    exposing
        ( create
        , destroy
        , indexForTable
        , one
        , update
        , updateWithConstraints
        )

import Data.Column as Column exposing (Column)
import Http exposing (Request)
import Json.Decode as JD
import Request.Table exposing (tableUrl)
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


columnsUrl : String
columnsUrl =
    baseUrl ++ "columns/"


columnUrl : Int -> String
columnUrl =
    toString >> (++) columnsUrl


tableColumnsUrl : Int -> String
tableColumnsUrl =
    tableUrl >> flip (++) "/columns"


create : Column -> Request Column
create column =
    Http.post
        columnsUrl
        (Column.encodeNew column |> Http.jsonBody)
        (dataDecoder <| Column.decoder)


one : Int -> Request Column
one id =
    Http.get (columnUrl id) (dataDecoder Column.decoder)


indexForTable : Int -> Request (List Column)
indexForTable tableId =
    Http.get
        (tableColumnsUrl tableId)
        (dataDecoder (JD.list Column.decoder))


update : Column -> Request Column
update column =
    put (columnUrl column.id)
        (Column.encode column |> Http.jsonBody)
        (dataDecoder Column.decoder)


updateWithConstraints : Column -> Request Column
updateWithConstraints column =
    put (columnUrl column.id)
        (Column.encode column |> Http.jsonBody)
        (dataDecoder Column.decoder)


destroy : Int -> Request ()
destroy id =
    delete (columnUrl id)
