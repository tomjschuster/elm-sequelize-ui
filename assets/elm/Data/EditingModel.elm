module Data.EditingModel
    exposing
        ( EditingModel
        , editField
        , editSchema
        , editTable
        , extractField
        , extractSchema
        , extractTable
        , isField
        , isSchema
        , isTable
        , mapField
        , mapSchema
        , mapTable
        , none
        , updateField
        , updateSchema
        , updateTable
        )

import Data.Field exposing (Field)
import Data.Schema exposing (Schema)
import Data.Table exposing (Table)


type EditingModel
    = NoModel
    | EditingSchema Schema
    | EditingTable Table
    | EditingField Field


none : EditingModel
none =
    NoModel


editSchema : Schema -> EditingModel
editSchema =
    EditingSchema


editTable : Table -> EditingModel
editTable =
    EditingTable


editField : Field -> EditingModel
editField =
    EditingField


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


isField : Int -> EditingModel -> Bool
isField id editingModel =
    case editingModel of
        EditingField field ->
            id == field.id

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


updateField : Field -> EditingModel -> EditingModel
updateField field editingModel =
    if isField field.id editingModel then
        EditingField field
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


extractField : EditingModel -> Maybe Field
extractField editingModel =
    case editingModel of
        EditingField field ->
            Just field

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


mapField : (a -> Field -> Field) -> a -> EditingModel -> EditingModel
mapField toField input editingModel =
    editingModel
        |> extractField
        |> Maybe.map (toField input >> EditingField)
        |> Maybe.withDefault editingModel
