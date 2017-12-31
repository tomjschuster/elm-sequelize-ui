module Request.Column
    exposing
        ( create
        , destroy
        , indexForSchema
        , indexForTable
        , indexForTableWithDataType
        , indexReferences
        , one
        , update
        , updateWithConstraints
        )

import Data.Column as Column exposing (Column)
import Data.Column.Constraints as ColumnConstraints exposing (ColumnConstraints)
import Data.DataType as DataType exposing (DataType)
import Http exposing (Request)
import Json.Decode as JD
import Json.Encode as JE
import Request.Schema as SchemaReq
import Request.Table as TableReq
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


url : String
url =
    baseUrl ++ "columns/"


resourceUrl : Int -> String
resourceUrl =
    toString >> (++) url


tableUrl : Int -> String
tableUrl =
    TableReq.resourceUrl >> flip (++) "/columns"


schemaUrl : Int -> String
schemaUrl =
    SchemaReq.resourceUrl >> flip (++) "/columns"


referencesUrl : Int -> String
referencesUrl =
    TableReq.resourceUrl >> flip (++) "/column-references"


tableColumnsForDataTypeUrl : Int -> DataType -> String
tableColumnsForDataTypeUrl tableId =
    DataType.toUrlParams >> (++) (TableReq.resourceUrl tableId ++ "?")


create : Column -> ColumnConstraints -> Request Column
create column constraints =
    Http.post
        url
        (JE.object
            [ ( "column", Column.encode column )
            , ( "constraints", ColumnConstraints.encode constraints )
            ]
            |> Http.jsonBody
        )
        (dataDecoder <| Column.decoder)


one : Int -> Request Column
one id =
    Http.get (resourceUrl id) (dataDecoder Column.decoder)


indexForTable : Int -> Request (List Column)
indexForTable tableId =
    Http.get
        (tableUrl tableId)
        (dataDecoder (JD.list Column.decoder))


indexForSchema : Int -> Request (List Column)
indexForSchema schemaId =
    Http.get
        (schemaUrl schemaId)
        (dataDecoder (JD.list Column.decoder))


indexForTableWithDataType : Int -> DataType -> Request (List Column)
indexForTableWithDataType tableId dataType =
    Http.get
        (tableColumnsForDataTypeUrl tableId dataType)
        (dataDecoder (JD.list Column.decoder))


indexReferences : Int -> Request (List Column)
indexReferences tableId =
    Http.get
        (referencesUrl tableId)
        (dataDecoder (JD.list Column.decoder))


update : Column -> Request Column
update column =
    put (resourceUrl column.id)
        (JE.object [ ( "column", Column.encode column ) ]
            |> Http.jsonBody
        )
        (dataDecoder Column.decoder)


updateWithConstraints : Column -> ColumnConstraints -> Request Column
updateWithConstraints column constraints =
    put (resourceUrl column.id)
        (JE.object
            [ ( "column", Column.encode column )
            , ( "constraints", ColumnConstraints.encode constraints )
            ]
            |> Http.jsonBody
        )
        (dataDecoder Column.decoder)


destroy : Int -> Request ()
destroy id =
    delete (resourceUrl id)
