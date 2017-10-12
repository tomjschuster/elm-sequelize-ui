module Data.ChangesetError exposing (ChangesetError, fieldToText, parseHttpError)

import Http
import Json.Decode as JD exposing (Decoder)
import Utils.Http exposing (errorBody, isUnprocessableTable)


type alias ChangesetError =
    { field : String, messages : List String }


fieldToText : String -> String
fieldToText fieldName =
    case fieldName of
        "name" ->
            "name"

        _ ->
            fieldName


parseHttpError : Http.Error -> List ChangesetError
parseHttpError =
    unprocessableTableBody
        >> Result.fromMaybe "error parsing HTTP error body"
        >> Result.andThen decodeErrorBody
        >> Result.withDefault []


unprocessableTableBody : Http.Error -> Maybe String
unprocessableTableBody error =
    if isUnprocessableTable error then
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
