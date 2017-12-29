module Request.Table
    exposing
        ( create
        , destroy
        , index
        , indexForSchema
        , indexReferenceCandidates
        , indexReferences
        , one
        , resourceUrl
        , update
        )

import Data.Column as Column exposing (Column)
import Data.Column.DataType as DataType exposing (DataType)
import Data.Table as Table exposing (Table)
import Http exposing (Request)
import Json.Decode as JD
import Request.Schema as SchemaReq
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


url : String
url =
    baseUrl ++ "tables/"


resourceUrl : Int -> String
resourceUrl =
    toString >> (++) url


schemaTablesUrl : Int -> String
schemaTablesUrl =
    SchemaReq.resourceUrl >> flip (++) "/tables"


referenceCandidatesUrl : Int -> DataType -> String
referenceCandidatesUrl schemaId =
    DataType.toUrlParams >> (++) (SchemaReq.resourceUrl schemaId ++ "/candidates?")



--schemaTablesUrl schemaId ++ "?data_type_id=" ++ toString dataTypeId


referencesUrl : Int -> String
referencesUrl =
    resourceUrl >> flip (++) "/table-references"



-- CREATE


create : Table -> Request Table
create table =
    Http.post url
        (Table.encodeNew table |> Http.jsonBody)
        (dataDecoder Table.decoder)



-- READ


index : Request (List Table)
index =
    Http.get url (dataDecoder (JD.list Table.decoder))


one : Int -> Request Table
one id =
    Http.get (resourceUrl id) (dataDecoder Table.decoder)


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


indexReferenceCandidates : Int -> DataType -> Http.Request ( List Table, List Column )
indexReferenceCandidates schemaId dataType =
    Http.get
        (referenceCandidatesUrl schemaId dataType)
        (dataDecoder
            (JD.map2 (,)
                (JD.list Table.decoder |> JD.field "tables")
                (JD.list Column.decoder |> JD.field "columns")
            )
        )



-- UPDATE


update : Table -> Request Table
update table =
    put
        (resourceUrl table.id)
        (Table.encode table |> Http.jsonBody)
        (dataDecoder Table.decoder)



-- DELETE


destroy : Int -> Request ()
destroy id =
    delete (resourceUrl id)
