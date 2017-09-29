module Request.Field exposing (create, destroy, one, update)

import Data.Field as Field exposing (Field)
import Http exposing (Request)
import Json.Encode as JE
import Utils.Http exposing (baseUrl, delete, put)


fieldsUrl : String
fieldsUrl =
    baseUrl ++ "fields/"


fieldUrl : Int -> String
fieldUrl =
    toString >> (++) fieldsUrl


create : String -> Int -> Request Field
create name entityId =
    Http.post
        fieldsUrl
        (JE.object
            [ ( "name", JE.string name )
            , ( "entityId", JE.int entityId )
            ]
            |> Http.jsonBody
        )
        Field.decoder


one : Int -> Request Field
one id =
    Http.get (fieldUrl id) Field.decoder


update : Field -> Request Field
update field =
    put (fieldUrl field.id) (Field.encode field |> Http.jsonBody) Field.decoder


destroy : Int -> Request ()
destroy id =
    delete (fieldUrl id)
