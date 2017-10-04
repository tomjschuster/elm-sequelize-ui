module Data.ChangesetError exposing (ChangesetError, parseHttpError)

import Http
import Json.Decode as JD exposing (Decoder)
import Utils.Http exposing (errorBody, isUnprocessableEntity)


type alias ChangesetError =
    { field : String, messages : List String }


parseHttpError : Http.Error -> List ChangesetError
parseHttpError =
    unprocessableEntityBody
        >> Result.fromMaybe "error parsing HTTP error body"
        >> Result.andThen decodeErrorBody
        >> Result.withDefault []



-->> Result.toMaybe


unprocessableEntityBody : Http.Error -> Maybe String
unprocessableEntityBody error =
    if isUnprocessableEntity error then
        errorBody error
    else
        Nothing


decodeErrorBody : String -> Result String (List ChangesetError)
decodeErrorBody =
    JD.decodeString decoder


decoder : Decoder (List ChangesetError)
decoder =
    JD.keyValuePairs (JD.list JD.string)
        |> JD.map (List.map (uncurry ChangesetError))
        |> JD.field "errors"
