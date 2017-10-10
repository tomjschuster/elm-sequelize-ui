module Request.Entity
    exposing
        ( all
        , create
        , destroy
        , one
        , oneWithAll
        , oneWithFields
        , oneWithSchema
        , update
        )

import Data.Combined as Combined exposing (EntityWithAll, EntityWithFields, EntityWithSchema)
import Data.Entity as Entity exposing (Entity)
import Http exposing (Request)
import Json.Decode as JD
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


entitiesUrl : String
entitiesUrl =
    baseUrl ++ "entities/"


withFields : String -> String
withFields =
    flip (++) "?" >> flip (++) "&fields=show"


withSchema : String -> String
withSchema =
    flip (++) "?" >> flip (++) "&schema=show"


withAll : String -> String
withAll =
    flip (++) "?" >> flip (++) "&schema=show" >> flip (++) "&fields=show"


entityUrl : Int -> String
entityUrl =
    toString >> (++) entitiesUrl



-- CREATE


create : Entity -> Request Entity
create entity =
    Http.post entitiesUrl
        (Entity.encodeNew entity |> Http.jsonBody)
        (dataDecoder Entity.decoder)



-- READ


all : Request (List Entity)
all =
    Http.get entitiesUrl (dataDecoder (JD.list Entity.decoder))


one : Int -> Request Entity
one id =
    Http.get (entityUrl id) (dataDecoder Entity.decoder)


oneWithSchema : Int -> Http.Request EntityWithSchema
oneWithSchema id =
    Http.get
        (entityUrl id |> withSchema)
        (dataDecoder Combined.entityWithSchemaDecoder)


oneWithFields : Int -> Http.Request EntityWithFields
oneWithFields id =
    Http.get
        (entityUrl id |> withFields)
        (dataDecoder Combined.entityWithFieldsDecoder)


oneWithAll : Int -> Http.Request EntityWithAll
oneWithAll id =
    Http.get
        (entityUrl id |> withAll)
        (dataDecoder Combined.entityWithAllDecoder)



-- UPDATE


update : Entity -> Request Entity
update entity =
    put
        (entityUrl entity.id)
        (Entity.encode entity |> Http.jsonBody)
        (dataDecoder Entity.decoder)



-- DELETE


destroy : Int -> Request ()
destroy id =
    delete (entityUrl id)
