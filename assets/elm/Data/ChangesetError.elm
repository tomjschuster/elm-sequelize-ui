module Data.ChangesetError
    exposing
        ( ChangesetError
        , columnToText
        , parseHttpError
        )

import Http
import Json.Decode as JD exposing (Decoder)
import Utils.Http exposing (errorBody, isUnprocessableTable)


type alias ChangesetError =
    { column : String
    , messages : List String
    }


columnToText : String -> String
columnToText columnName =
    case columnName of
        "name" ->
            "name"

        _ ->
            columnName


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
