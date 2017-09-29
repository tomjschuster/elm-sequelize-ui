module Request.Entity exposing (all, create, destroy, one, oneWithFields, update)

import Data.Entity as Entity exposing (Entity)
import Http exposing (Request)
import Json.Decode as JD
import Json.Encode as JE
import Utils.Http exposing (baseUrl, delete, put)


entitiesUrl : String
entitiesUrl =
    baseUrl ++ "entities/"


embedFields : String -> String
embedFields =
    flip (++) "?_embed=fields"


entityUrl : Int -> String
entityUrl =
    toString >> (++) entitiesUrl


all : Request (List Entity)
all =
    Http.get entitiesUrl (JD.list Entity.decoder)


one : Int -> Request Entity
one id =
    Http.get (entityUrl id) Entity.decoder


oneWithFields : Int -> Http.Request Entity
oneWithFields id =
    Http.get (entityUrl id |> embedFields) Entity.decoder


create : String -> Int -> Request Entity
create name schemaId =
    Http.post entitiesUrl
        (JE.object
            [ ( "name", JE.string name )
            , ( "schemaId", JE.int schemaId )
            ]
            |> Http.jsonBody
        )
        Entity.decoder


destroy : Int -> Request ()
destroy id =
    delete (entityUrl id)


update : Entity -> Request Entity
update entity =
    put
        (entityUrl entity.id)
        (Entity.encode entity |> Http.jsonBody)
        Entity.decoder
