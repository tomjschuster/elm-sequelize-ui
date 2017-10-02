module Request.Schema
    exposing
        ( all
        , create
        , destroy
        , one
        , oneWithEntities
        , update
        )

import Data.Combined as Combined exposing (SchemaWithEntities)
import Data.Schema as Schema exposing (Schema)
import Http
import Json.Decode as JD
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


schemasUrl : String
schemasUrl =
    baseUrl ++ "schemas/"


withEntities : String -> String
withEntities =
    flip (++) "?entities=show"


schemaUrl : Int -> String
schemaUrl =
    toString >> (++) schemasUrl


all : Http.Request (List Schema)
all =
    Http.get
        schemasUrl
        (dataDecoder (JD.list Schema.decoder))


oneWithEntities : Int -> Http.Request SchemaWithEntities
oneWithEntities id =
    Http.get
        (schemaUrl id |> withEntities)
        (dataDecoder Combined.schemaWithEntitiesDecoder)


one : Int -> Http.Request Schema
one id =
    Http.get (schemaUrl id) Schema.decoder


create : String -> Http.Request Schema
create name =
    Http.post
        schemasUrl
        (Schema.encodeNewSchema name |> Http.jsonBody)
        (dataDecoder Schema.decoder)


update : Schema -> Http.Request Schema
update schema =
    put (schemaUrl schema.id)
        (Http.jsonBody (Schema.encode schema))
        (dataDecoder Schema.decoder)


destroy : Int -> Http.Request ()
destroy id =
    delete (schemaUrl id)
