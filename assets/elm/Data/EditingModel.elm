module Data.EditingModel
    exposing
        ( EditingModel
        , editColumn
        , editSchema
        , editTable
        , extractColumn
        , extractSchema
        , extractTable
        , isColumn
        , isSchema
        , isTable
        , mapColumn
        , mapSchema
        , mapTable
        , none
        , updateColumn
        , updateSchema
        , updateTable
        )

import Data.Column exposing (Column)
import Data.Schema exposing (Schema)
import Data.Table exposing (Table)


type EditingModel
    = NoModel
    | EditingSchema Schema
    | EditingTable Table
    | EditingColumn Column


none : EditingModel
none =
    NoModel


editSchema : Schema -> EditingModel
editSchema =
    EditingSchema


editTable : Table -> EditingModel
editTable =
    EditingTable


editColumn : Column -> EditingModel
editColumn =
    EditingColumn


isSchema : Int -> EditingModel -> Bool
isSchema id editingModel =
    case editingModel of
        EditingSchema schema ->
            id == schema.id

        _ ->
            False


isTable : Int -> EditingModel -> Bool
isTable id editingModel =
    case editingModel of
        EditingTable table ->
            id == table.id

        _ ->
            False


isColumn : Int -> EditingModel -> Bool
isColumn id editingModel =
    case editingModel of
        EditingColumn column ->
            id == column.id

        _ ->
            False


updateSchema : Schema -> EditingModel -> EditingModel
updateSchema schema editingModel =
    if isSchema schema.id editingModel then
        EditingSchema schema
    else
        editingModel


updateTable : Table -> EditingModel -> EditingModel
updateTable table editingModel =
    if isTable table.id editingModel then
        EditingTable table
    else
        editingModel


updateColumn : Column -> EditingModel -> EditingModel
updateColumn column editingModel =
    if isColumn column.id editingModel then
        EditingColumn column
    else
        editingModel


extractSchema : EditingModel -> Maybe Schema
extractSchema editingModel =
    case editingModel of
        EditingSchema schema ->
            Just schema

        _ ->
            Nothing


extractTable : EditingModel -> Maybe Table
extractTable editingModel =
    case editingModel of
        EditingTable table ->
            Just table

        _ ->
            Nothing


extractColumn : EditingModel -> Maybe Column
extractColumn editingModel =
    case editingModel of
        EditingColumn column ->
            Just column

        _ ->
            Nothing


mapSchema : (a -> Schema -> Schema) -> a -> EditingModel -> EditingModel
mapSchema toSchema input editingModel =
    editingModel
        |> extractSchema
        |> Maybe.map (toSchema input >> EditingSchema)
        |> Maybe.withDefault editingModel


mapTable : (a -> Table -> Table) -> a -> EditingModel -> EditingModel
mapTable toTable input editingModel =
    editingModel
        |> extractTable
        |> Maybe.map (toTable input >> EditingTable)
        |> Maybe.withDefault editingModel


mapColumn : (a -> Column -> Column) -> a -> EditingModel -> EditingModel
mapColumn toColumn input editingModel =
    editingModel
        |> extractColumn
        |> Maybe.map (toColumn input >> EditingColumn)
        |> Maybe.withDefault editingModel
