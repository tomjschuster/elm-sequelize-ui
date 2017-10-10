module Data.EditingModel
    exposing
        ( EditingModel
        , editEntity
        , editField
        , editSchema
        , extractEntity
        , extractField
        , extractSchema
        , isEntity
        , isField
        , isSchema
        , mapEntity
        , mapField
        , mapSchema
        , none
        , updateEntity
        , updateField
        , updateSchema
        )

import Data.Entity exposing (Entity)
import Data.Field exposing (Field)
import Data.Schema exposing (Schema)


type EditingModel
    = NoModel
    | EditingSchema Schema
    | EditingEntity Entity
    | EditingField Field


none : EditingModel
none =
    NoModel


editSchema : Schema -> EditingModel
editSchema =
    EditingSchema


editEntity : Entity -> EditingModel
editEntity =
    EditingEntity


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


isEntity : Int -> EditingModel -> Bool
isEntity id editingModel =
    case editingModel of
        EditingEntity entity ->
            id == entity.id

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


updateEntity : Entity -> EditingModel -> EditingModel
updateEntity entity editingModel =
    if isEntity entity.id editingModel then
        EditingEntity entity
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


extractEntity : EditingModel -> Maybe Entity
extractEntity editingModel =
    case editingModel of
        EditingEntity entity ->
            Just entity

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


mapEntity : (a -> Entity -> Entity) -> a -> EditingModel -> EditingModel
mapEntity toEntity input editingModel =
    editingModel
        |> extractEntity
        |> Maybe.map (toEntity input >> EditingEntity)
        |> Maybe.withDefault editingModel


mapField : (a -> Field -> Field) -> a -> EditingModel -> EditingModel
mapField toField input editingModel =
    editingModel
        |> extractField
        |> Maybe.map (toField input >> EditingField)
        |> Maybe.withDefault editingModel
