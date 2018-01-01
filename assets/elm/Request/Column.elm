module Request.Column
    exposing
        ( create
        , destroy
        , indexForSchema
        , one
        , updateWithConstraints
        )

import Data.Column as Column exposing (Column)
import Data.Column.Constraints as ColumnConstraints exposing (ColumnConstraints)
import Http exposing (Request)
import Json.Decode as JD
import Json.Encode as JE
import Request.Schema as SchemaReq
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


url : String
url =
    baseUrl ++ "columns/"


resourceUrl : Int -> String
resourceUrl =
    toString >> (++) url


schemaUrl : Int -> String
schemaUrl =
    SchemaReq.resourceUrl >> flip (++) "/columns"


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


indexForSchema : Int -> Request (List Column)
indexForSchema schemaId =
    Http.get
        (schemaUrl schemaId)
        (dataDecoder (JD.list Column.decoder))


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
