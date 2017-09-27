module Request.Schema
    exposing
        ( all
        , create
        , destroy
        , one
        , oneWithEntities
        , update
        )

import Data.Schema as Schema exposing (Schema)
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
    Http.get (embedEntities schemasUrl) (JD.list Schema.decoder)


oneWithEntities : Int -> Http.Request Schema
oneWithEntities id =
    Http.get (schemaUrl id |> embedEntities) Schema.decoder


one : Int -> Http.Request Schema
one id =
    Http.get (schemaUrl id) Schema.decoder


create : String -> Http.Request Schema
create name =
    Http.post
        schemasUrl
        (JE.object [ ( "name", JE.string name ) ] |> Http.jsonBody)
        Schema.decoder


update : Schema -> Http.Request Schema
update schema =
    put (schemaUrl schema.id) (Http.jsonBody (Schema.encode schema)) Schema.decoder


destroy : Int -> Http.Request ()
destroy id =
    delete (schemaUrl id)
