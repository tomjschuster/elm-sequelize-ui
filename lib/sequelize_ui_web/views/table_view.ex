defmodule SequelizeUiWeb.TableView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{TableView, SchemaView, ColumnView, ConstraintView}

  def render("index.json", %{entities: entities}) do
    %{data: render_many(entities, TableView, "table.json")}
  end

  def render("show.json", %{table: table}) do
    %{data: render_one(table, TableView, "table.json")}
  end

  def render("show-with-all.json", %{table: table}) do
    IO.inspect table
    %{data: %{table: render(TableView, "table-with-constraints.json", table: table),
              schema: render_one(table.schema, SchemaView, "schema.json"),
              columns: render_many(table.columns, ColumnView, "column.json")}}
              # constraints: render(ConstraintView, "table-constraints.json", constraints: table.constraints)}}
  end

  def render("show-with-schema.json", %{table: table}) do
    %{data: %{table: render_one(table, TableView, "table.json"),
              schema: render_one(table.schema, SchemaView, "schema.json")}}
  end

  def render("show-with-columns.json", %{table: table}) do
    %{data: %{table: render_one(table, TableView, "table.json"),
              columns: render_many(table.columns, ColumnView, "column.json")}}
  end

  def render("table.json", %{table: table}) do
    %{id: table.id,
      name: table.name,
      schemaId: table.schema_id}
  end

  def render("table-with-constraints.json", %{table: table}) do
    %{id: table.id,
      name: table.name,
      constraints: render(ConstraintView, "table-constraints.json", constraints: table.constraints)}
  end
end
