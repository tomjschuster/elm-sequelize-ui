module Request.Column
    exposing
        ( create
        , destroy
        , indexForTable
        , indexReferences
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


referencesUrl : Int -> String
referencesUrl =
    tableUrl >> flip (++) "/column-references"


create : Column -> List Int -> Request Column
create column referenceIds =
    Http.post
        columnsUrl
        (Column.encodeNew column referenceIds |> Http.jsonBody)
        (dataDecoder <| Column.decoder)


one : Int -> Request Column
one id =
    Http.get (columnUrl id) (dataDecoder Column.decoder)


indexForTable : Int -> Request (List Column)
indexForTable tableId =
    Http.get
        (tableColumnsUrl tableId)
        (dataDecoder (JD.list Column.decoder))


indexReferences : Int -> Request (List Column)
indexReferences tableId =
    Http.get
        (referencesUrl tableId)
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
