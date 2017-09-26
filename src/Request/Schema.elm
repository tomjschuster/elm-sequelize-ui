module Request.Schema
    exposing
        ( create
        , destroy
        , get
        , getAll
        , update
        )

import Data.Schema exposing (Schema, encodeSchema, schemaDecoder)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Utils.Http exposing (baseUrl, delete, put)


schemasUrl : String
schemasUrl =
    baseUrl ++ "schemas/"


schemaUrl : Int -> String
schemaUrl =
    toString >> (++) schemasUrl


getAll : Http.Request (List Schema)
getAll =
    Http.get schemasUrl (JD.list schemaDecoder)


create : String -> Http.Request Schema
create name =
    Http.post schemasUrl (JE.object [ ( "name", JE.string name ) ] |> Http.jsonBody) schemaDecoder


get : Int -> Http.Request Schema
get id =
    Http.get (schemaUrl id) schemaDecoder


update : Schema -> Http.Request Schema
update schema =
    put (schemaUrl schema.id) (Http.jsonBody (encodeSchema schema)) schemaDecoder


destroy : Int -> Http.Request ()
destroy id =
    delete (schemaUrl id)
