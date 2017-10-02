module Utils.Http exposing (baseUrl, dataDecoder, delete, put)

import Http
import Json.Decode as JD exposing (Decoder)


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


dataDecoder : Decoder a -> Decoder a
dataDecoder decoder =
    JD.field "data" decoder
