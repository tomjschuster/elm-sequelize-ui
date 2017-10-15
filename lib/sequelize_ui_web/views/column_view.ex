defmodule SequelizeUiWeb.ColumnView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{ColumnView, TableView, SchemaView}

  def render("index.json", %{columns: columns}) do
    %{data: render_many(columns, ColumnView, "column.json")}
  end

  def render("show.json", %{column: column}) do
    %{data: render_one(column, ColumnView, "column.json")}
  end

  def render("show-with-table.json", %{column: column}) do
    %{data: %{column: render_one(column, ColumnView, "column.json"),
              table: render_one(column.table, TableView, "table.json")}}
  end

  def render("show-with-all.json", %{column: column}) do
    %{data: %{column: render_one(column, ColumnView, "column.json"),
              table: render_one(column.table, TableView, "table.json"),
              schema: render_one(column.table.schema, SchemaView, "schema.json")}}
  end

  def render("column.json", %{column: column}) do
    %{id: column.id,
      name: column.name,
      tableId: column.table_id,
      dataTypeId: column.data_type_id,
      size: column.size,
      precision: column.precision,
      scale: column.scale,
      withTimezone: column.with_timezone}
  end
end
