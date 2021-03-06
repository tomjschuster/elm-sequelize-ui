module Request.Table
    exposing
        ( create
        , destroy
        , index
        , indexForSchema
        , one
        , resourceUrl
        , update
        )

import Data.Table as Table exposing (Table)
import Http exposing (Request)
import Json.Decode as JD
import Json.Encode as JE
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



-- CREATE


create : Table -> Request Table
create table =
    Http.post url
        (JE.object [ ( "table", Table.encodeNew table ) ]
            |> Http.jsonBody
        )
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



-- UPDATE


update : Table -> Request Table
update table =
    put
        (resourceUrl table.id)
        (JE.object [ ( "table", Table.encode table ) ]
            |> Http.jsonBody
        )
        (dataDecoder Table.decoder)



-- DELETE


destroy : Int -> Request ()
destroy id =
    delete (resourceUrl id)
