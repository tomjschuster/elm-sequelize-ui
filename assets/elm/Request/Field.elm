module Request.Field exposing (create, destroy, one, oneWithAll, oneWithEntity, update)

import Data.Combined as Combined exposing (FieldWithAll, FieldWithEntity)
import Data.DataType as DataType exposing (DataType)
import Data.Field as Field exposing (Field)
import Http exposing (Request)
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


fieldsUrl : String
fieldsUrl =
    baseUrl ++ "fields/"


withEntity : String -> String
withEntity =
    flip (++) "?" >> flip (++) "&entity=show"


withSchema : String -> String
withSchema =
    flip (++) "?" >> flip (++) "&schema=show"


withAll : String -> String
withAll =
    flip (++) "?" >> flip (++) "&schema=show" >> flip (++) "&entity=show"


fieldUrl : Int -> String
fieldUrl =
    toString >> (++) fieldsUrl


create : Int -> String -> DataType -> DataType.Modifier -> Request Field
create entityId name dataType modifier =
    Http.post
        fieldsUrl
        (Field.encodeNewField entityId name dataType modifier |> Http.jsonBody)
        (dataDecoder Field.decoder)


one : Int -> Request Field
one id =
    Http.get (fieldUrl id) (dataDecoder Field.decoder)


oneWithEntity : Int -> Request FieldWithEntity
oneWithEntity id =
    Http.get (fieldUrl id |> withEntity) (dataDecoder Combined.fieldWithEntityDecoder)


oneWithAll : Int -> Request FieldWithAll
oneWithAll id =
    Http.get (fieldUrl id |> withAll) (dataDecoder Combined.fieldWithAllDecoder)


update : Field -> Request Field
update field =
    put (fieldUrl field.id)
        (Field.encode field |> Http.jsonBody)
        (dataDecoder Field.decoder)


destroy : Int -> Request ()
destroy id =
    delete (fieldUrl id)
