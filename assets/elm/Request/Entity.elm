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

import Data.Entity as Entity exposing (Entity)
import Data.Schema as Schema exposing (Schema)
import Http exposing (Request)
import Json.Decode as JD
import Json.Encode as JE
import Utils.Http exposing (baseUrl, delete, put)


entitiesUrl : String
entitiesUrl =
    baseUrl ++ "entities/"


withAssociations : String -> String
withAssociations =
    flip (++) "?"


withFields : String -> String
withFields =
    flip (++) "&entities=show"


withSchema : String -> String
withSchema =
    flip (++) "&schema=show"


entityUrl : Int -> String
entityUrl =
    toString >> (++) entitiesUrl



-- CREATE


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



-- READ


all : Request (List Entity)
all =
    Http.get entitiesUrl (JD.list Entity.decoder)


one : Int -> Request Entity
one id =
    Http.get (entityUrl id) Entity.decoder


oneWithSchema : Int -> Http.Request Entity
oneWithSchema id =
    Http.get
        (entityUrl id |> withAssociations |> withSchema)
        Entity.decoder


oneWithFields : Int -> Http.Request Entity
oneWithFields id =
    Http.get
        (entityUrl id |> withAssociations |> withFields)
        Entity.decoder


oneWithAll : Int -> Http.Request Entity
oneWithAll id =
    Http.get
        (entityUrl id |> withAssociations |> withSchema |> withFields)
        Entity.decoder



-- UPDATE


update : Entity -> Request Entity
update entity =
    put
        (entityUrl entity.id)
        (Entity.encode entity |> Http.jsonBody)
        Entity.decoder



-- DELETE


destroy : Int -> Request ()
destroy id =
    delete (entityUrl id)
