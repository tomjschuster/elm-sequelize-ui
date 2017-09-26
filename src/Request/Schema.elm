module Request.Schema
    exposing
        ( all
        , create
        , destroy
        , one
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


embedEntities : String -> String
embedEntities =
    flip (++) "?_embed=entities"


schemaUrl : Int -> String
schemaUrl =
    toString >> (++) schemasUrl


all : Http.Request (List Schema)
all =
    Http.get (embedEntities schemasUrl) (JD.list schemaDecoder)


one : Int -> Http.Request Schema
one id =
    Http.get (schemaUrl id |> embedEntities) schemaDecoder


create : String -> Http.Request Schema
create name =
    Http.post
        schemasUrl
        (JE.object [ ( "name", JE.string name ) ] |> Http.jsonBody)
        schemaDecoder


update : Schema -> Http.Request Schema
update schema =
    put (schemaUrl schema.id) (Http.jsonBody (encodeSchema schema)) schemaDecoder


destroy : Int -> Http.Request ()
destroy id =
    delete (schemaUrl id)
