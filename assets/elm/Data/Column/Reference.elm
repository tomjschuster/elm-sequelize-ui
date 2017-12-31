module Data.Column.Reference
    exposing
        ( Data
        , Reference(..)
        , display
        , encode
        , fromColumnId
        , isComplete
        , selectColumn
        , selectTable
        , start
        )

import Data.Column exposing (Column)
import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)


type alias Data =
    { columnId : Int
    , columnName : String
    , tableId : Int
    , tableName : String
    }


type Reference
    = None
    | TableSelected Int
    | Complete Int Int


fromColumnId : Dict Int Column -> Int -> Maybe Reference
fromColumnId columnLookup columnId =
    columnLookup
        |> Dict.get columnId
        |> Maybe.map (.tableId >> flip Complete columnId)


start : Reference
start =
    None


isComplete : Reference -> Bool
isComplete reference =
    case reference of
        Complete _ _ ->
            True

        _ ->
            False


selectColumn : Maybe Int -> Reference -> Reference
selectColumn maybeColumnId reference =
    case reference of
        TableSelected tableId ->
            maybeColumnId
                |> Maybe.map (Complete tableId)
                |> Maybe.withDefault (TableSelected tableId)

        _ ->
            reference


selectTable : Maybe Int -> Reference -> Reference
selectTable maybeTableId reference =
    if getTableId reference /= maybeTableId then
        maybeTableId
            |> Maybe.map TableSelected
            |> Maybe.withDefault None
    else
        reference


getColumnId : Reference -> Maybe Int
getColumnId reference =
    case reference of
        Complete _ columnId ->
            Just columnId

        _ ->
            Nothing


display : String -> String -> String
display tableName columnName =
    "references " ++ tableName ++ "(" ++ columnName ++ ")"


encode : Reference -> Value
encode =
    getColumnId >> Maybe.map JE.int >> Maybe.withDefault JE.null


getTableId : Reference -> Maybe Int
getTableId reference =
    case reference of
        TableSelected tableId ->
            Just tableId

        Complete tableId _ ->
            Just tableId

        _ ->
            Nothing
