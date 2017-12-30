module Data.Column.Reference
    exposing
        ( Data
        , Reference(..)
        , encode
        , maybeToString
        , selectColumn
        , selectTable
        , start
        , toString
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


maybeToString : Maybe Reference -> String
maybeToString =
    Maybe.andThen toString >> Maybe.withDefault ""


toString : Reference -> Maybe String
toString reference =
    case reference of
        Display _ tableName _ columnName ->
            Just (buildString tableName columnName)

        _ ->
            Nothing


buildString : String -> String -> String
buildString tableName columnName =
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
