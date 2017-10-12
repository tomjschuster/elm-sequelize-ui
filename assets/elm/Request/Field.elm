module Request.Field exposing (create, destroy, one, oneWithAll, oneWithTable, update)

import Data.Combined as Combined exposing (FieldWithAll, FieldWithTable)
import Data.DataType as DataType exposing (DataType)
import Data.Field as Field exposing (Field)
import Http exposing (Request)
import Utils.Http exposing (baseUrl, dataDecoder, delete, put)


fieldsUrl : String
fieldsUrl =
    baseUrl ++ "fields/"


withTable : String -> String
withTable =
    flip (++) "?" >> flip (++) "&table=show"


withSchema : String -> String
withSchema =
    flip (++) "?" >> flip (++) "&schema=show"


withAll : String -> String
withAll =
    flip (++) "?" >> flip (++) "&schema=show" >> flip (++) "&table=show"


fieldUrl : Int -> String
fieldUrl =
    toString >> (++) fieldsUrl


create : Field -> Request Field
create field =
    Http.post
        fieldsUrl
        (Field.encodeNew field |> Http.jsonBody)
        (dataDecoder Field.decoder)


one : Int -> Request Field
one id =
    Http.get (fieldUrl id) (dataDecoder Field.decoder)


oneWithTable : Int -> Request FieldWithTable
oneWithTable id =
    Http.get (fieldUrl id |> withTable) (dataDecoder Combined.fieldWithTableDecoder)


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
