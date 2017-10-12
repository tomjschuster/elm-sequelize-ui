defmodule SequelizeUiWeb.SchemaView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{SchemaView, TableView}

  def render("index.json", %{schemas: schemas}) do
    %{data: render_many(schemas, SchemaView, "schema.json")}
  end

  def render("show.json", %{schema: schema}) do
    %{data: render_one(schema, SchemaView, "schema.json")}
  end

  def render("show-with-tables.json", %{schema: schema}) do
    %{data: %{schema: render_one(schema, SchemaView, "schema.json"),
              tables: render_many(schema.tables, TableView, "table.json")}}
  end

  def render("schema.json", %{schema: schema}) do
    %{id: schema.id,
      name: schema.name}
  end
end
