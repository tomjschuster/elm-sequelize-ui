module Utils.Http
    exposing
        ( baseUrl
        , dataDecoder
        , delete
        , errorBody
        , isUnprocessableEntity
        , isUnprocessableTable
        , put
        )

import Http
import Json.Decode as JD exposing (Decoder, list, string)


delete : String -> Http.Request ()
delete url =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


put : String -> Http.Body -> JD.Decoder a -> Http.Request a
put url body decoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


baseUrl : String
baseUrl =
    "http://localhost:4000/api/"


type HttpError
    = BadRequest
    | Unauthorized
    | Forbidden
    | NotFound
    | MethodNotAllowed
    | NotAcceptable
    | ImATeapot
    | UnprocessableTable


httpErrorToStatusCode : HttpError -> Int
httpErrorToStatusCode error =
    case error of
        BadRequest ->
            400

        Unauthorized ->
            401

        Forbidden ->
            403

        NotFound ->
            404

        MethodNotAllowed ->
            405

        NotAcceptable ->
            406

        ImATeapot ->
            418

        UnprocessableTable ->
            422


isError : HttpError -> Http.Error -> Bool
isError errorType httpError =
    case httpError of
        Http.BadStatus { status, body } ->
            if status.code == httpErrorToStatusCode errorType then
                True
            else
                False

        _ ->
            False


isUnprocessableTable : Http.Error -> Bool
isUnprocessableTable =
    isError UnprocessableTable


errorBody : Http.Error -> Maybe String
errorBody error =
    case error of
        Http.BadStatus { body } ->
            Just body

        _ ->
            Nothing


dataDecoder : Decoder a -> Decoder a
dataDecoder decoder =
    JD.field "data" decoder


isUnprocessableEntity : Http.Error -> Bool
isUnprocessableEntity error =
    case error of
        Http.BadStatus { status } ->
            status.code == 422

        _ ->
            False
