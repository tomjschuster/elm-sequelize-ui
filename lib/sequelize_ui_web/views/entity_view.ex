defmodule SequelizeUiWeb.EntityView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{EntityView, SchemaView, FieldView}

  def render("index.json", %{entities: entities}) do
    %{data: render_many(entities, EntityView, "entity.json")}
  end

  def render("show.json", %{entity: entity}) do
    %{data: render_one(entity, EntityView, "entity.json")}
  end

  def render("show-with-all.json", %{entity: entity}) do
    %{data: %{entity: render_one(entity, EntityView, "entity.json"),
              schema: render_one(entity.schema, SchemaView, "schema.json"),
              fields: render_many(entity.fields, FieldView, "field.json")}}
  end

  def render("show-with-schema.json", %{entity: entity}) do
    %{data: %{entity: render_one(entity, EntityView, "entity.json"),
              schema: render_one(entity.schema, SchemaView, "schema.json")}}
  end

  def render("show-with-fields.json", %{entity: entity}) do
    %{data: %{entity: render_one(entity, EntityView, "entity.json"),
              fields: render_many(entity.fields, FieldView, "field.json")}}
  end

  def render("entity.json", %{entity: entity}) do
    %{id: entity.id,
      name: entity.name,
      schemaId: entity.schema_id}
  end
end
