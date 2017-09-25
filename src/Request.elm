module Request exposing (createSchema, getSchema, getSchemas)

import Data exposing (Schema, schemaDecoder)
import Http
import Json.Decode as JD
import Json.Encode as JE


baseUrl : String
baseUrl =
    "http://localhost:4000/"


schemasUrl : String
schemasUrl =
    baseUrl ++ "schemas/"


schemaUrl : Int -> String
schemaUrl =
    toString >> (++) schemasUrl


getSchemas : Http.Request (List Schema)
getSchemas =
    Http.get schemasUrl (JD.list schemaDecoder)


createSchema : String -> Http.Request Schema
createSchema name =
    Http.post schemasUrl (JE.object [ ( "name", JE.string name ) ] |> Http.jsonBody) schemaDecoder


getSchema : Int -> Http.Request Schema
getSchema id =
    Http.get (schemaUrl id) schemaDecoder
