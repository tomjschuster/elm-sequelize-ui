defmodule SequelizeUiWeb.TableView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{TableView, SchemaView, FieldView}

  def render("index.json", %{entities: entities}) do
    %{data: render_many(entities, TableView, "table.json")}
  end

  def render("show.json", %{table: table}) do
    %{data: render_one(table, TableView, "table.json")}
  end

  def render("show-with-all.json", %{table: table}) do
    %{data: %{table: render_one(table, TableView, "table.json"),
              schema: render_one(table.schema, SchemaView, "schema.json"),
              fields: render_many(table.fields, FieldView, "field.json")}}
  end

  def render("show-with-schema.json", %{table: table}) do
    %{data: %{table: render_one(table, TableView, "table.json"),
              schema: render_one(table.schema, SchemaView, "schema.json")}}
  end

  def render("show-with-fields.json", %{table: table}) do
    %{data: %{table: render_one(table, TableView, "table.json"),
              fields: render_many(table.fields, FieldView, "field.json")}}
  end

  def render("table.json", %{table: table}) do
    %{id: table.id,
      name: table.name,
      schemaId: table.schema_id}
  end
end
