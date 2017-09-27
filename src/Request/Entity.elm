module Request.Entity exposing (all, create, destroy, one, update)

import Data.Entity exposing (Entity, encodeEntity, entityDecoder)
import Http exposing (Request)
import Json.Decode as JD
import Json.Encode as JE
import Utils.Http exposing (baseUrl, delete, put)


entitiesUrl : String
entitiesUrl =
    baseUrl ++ "entities/"


entityUrl : Int -> String
entityUrl =
    toString >> (++) entitiesUrl


all : Request (List Entity)
all =
    Http.get entitiesUrl (JD.list entityDecoder)


one : Int -> Request Entity
one id =
    Http.get (entityUrl id) entityDecoder


create : String -> Int -> Request Entity
create name schemaId =
    Http.post entitiesUrl
        (JE.object
            [ ( "name", JE.string name )
            , ( "schemaId", JE.int schemaId )
            ]
            |> Http.jsonBody
        )
        entityDecoder


destroy : Int -> Request ()
destroy id =
    delete (entityUrl id)


update : Entity -> Request Entity
update entity =
    put
        (entityUrl entity.id)
        (encodeEntity entity |> Http.jsonBody)
        entityDecoder
