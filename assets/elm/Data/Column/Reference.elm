module Data.Column.Reference
    exposing
        ( Data
        , Reference(..)
        , add
        , delete
        , encode
        , listToString
        , selectColumn
        , selectTable
        , singleToString
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


add : List Reference -> List Reference
add =
    flip (++) [ SelectTable ]


selectColumn : Int -> Maybe Int -> List Reference -> List Reference
selectColumn idx maybeColumnId =
    List.indexedMap
        (\currIdx reference ->
            case ( idx == currIdx, reference ) of
                ( True, SelectColumn tableId ) ->
                    maybeColumnId
                        |> Maybe.map (Ready tableId)
                        |> Maybe.withDefault (SelectColumn tableId)

                _ ->
                    reference
        )


selectTable : Int -> Maybe Int -> List Reference -> List Reference
selectTable idx maybeTableId =
    List.indexedMap
        (\currIdx reference ->
            if idx == currIdx && getTableId reference /= maybeTableId then
                maybeTableId
                    |> Maybe.map SelectColumn
                    |> Maybe.withDefault SelectTable
            else
                reference
        )


delete : Int -> List Reference -> List Reference
delete idx =
    List.indexedMap (,)
        >> List.filter (Tuple.first >> (/=) idx)
        >> List.map Tuple.second


getColumnId : Reference -> Maybe Int
getColumnId reference =
    case reference of
        Ready tableId columnId ->
            Just columnId

        Display tableId tableName columnId columnName ->
            Just columnId

        _ ->
            Nothing


listToString : List Reference -> String
listToString references =
    "references: " ++ (List.map singleToString references |> String.join ", ")


singleToString : Reference -> String
singleToString reference =
    case reference of
        Display tableId tableName columnId columnName ->
            tableName ++ "(" ++ columnName ++ ")"

        _ ->
            ""


encode : List Reference -> Value
encode =
    List.filterMap (getColumnId >> Maybe.map JE.int) >> JE.list


getTableId : Reference -> Maybe Int
getTableId reference =
    case reference of
        SelectColumn tableId ->
            Just tableId

        Ready tableId columnId ->
            Just tableId

        Display tableId tableName columnId columnName ->
            Just tableId

        _ ->
            Nothing
