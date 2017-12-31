module Request.Schema
    exposing
        ( create
        , destroy
        , index
        , one
        , resourceUrl
        , update
        , url
        )

import Data.Schema as Schema exposing (Schema)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


url : String
url =
    baseUrl ++ "schemas/"


resourceUrl : Int -> String
resourceUrl =
    toString >> (++) url


index : Http.Request (List Schema)
index =
    Http.get
        url
        (dataDecoder (JD.list Schema.decoder))


one : Int -> Http.Request Schema
one id =
    Http.get (resourceUrl id) (dataDecoder Schema.decoder)


create : Schema -> Http.Request Schema
create schema =
    Http.post
        url
        (JE.object [ ( "schema", Schema.encodeNew schema ) ]
            |> Http.jsonBody
        )
        (dataDecoder Schema.decoder)


update : Schema -> Http.Request Schema
update schema =
    put (resourceUrl schema.id)
        (JE.object [ ( "schema", Schema.encode schema ) ]
            |> Http.jsonBody
        )
        (dataDecoder Schema.decoder)


destroy : Int -> Http.Request ()
destroy id =
    delete (resourceUrl id)
