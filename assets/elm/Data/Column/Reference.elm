module Data.Column.Reference
    exposing
        ( Data
        , Reference(..)
        , display
        , encode
        , isDisplayable
        , selectColumn
        , selectTable
        , start
        )

import Json.Encode as JE exposing (Value)


type alias Data =
    { columnId : Int
    , columnName : String
    , tableId : Int
    , tableName : String
    }


type Reference
    = SelectTable
    | SelectColumn Int
    | Ready Int Int
    | Display Int String Int String


start : Reference
start =
    SelectTable


isDisplayable : Reference -> Bool
isDisplayable reference =
    case reference of
        Display _ _ _ _ ->
            True

        _ ->
            False


selectColumn : Maybe Int -> Reference -> Reference
selectColumn maybeColumnId reference =
    case reference of
        SelectColumn tableId ->
            maybeColumnId
                |> Maybe.map (Ready tableId)
                |> Maybe.withDefault (SelectColumn tableId)

        _ ->
            reference


selectTable : Maybe Int -> Reference -> Reference
selectTable maybeTableId reference =
    if getTableId reference /= maybeTableId then
        maybeTableId
            |> Maybe.map SelectColumn
            |> Maybe.withDefault SelectTable
    else
        reference


getColumnId : Reference -> Maybe Int
getColumnId reference =
    case reference of
        Ready _ columnId ->
            Just columnId

        Display _ _ columnId _ ->
            Just columnId

        _ ->
            Nothing


display : Reference -> String
display reference =
    case reference of
        Display _ tableName _ columnName ->
            buildDisplay tableName columnName

        _ ->
            ""


buildDisplay : String -> String -> String
buildDisplay tableName columnName =
    "references " ++ tableName ++ "(" ++ columnName ++ ")"


encode : Reference -> Value
encode =
    getColumnId >> Maybe.map JE.int >> Maybe.withDefault JE.null


getTableId : Reference -> Maybe Int
getTableId reference =
    case reference of
        SelectColumn tableId ->
            Just tableId

        Ready tableId _ ->
            Just tableId

        Display tableId _ _ _ ->
            Just tableId

        _ ->
            Nothing
