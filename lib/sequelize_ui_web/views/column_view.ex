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
    modifier =
      case {column.size, column.precision, column.decimals, column.with_timezone} do
        {nil, nil, nil, nil} ->
          nil
        {size, nil, nil, nil} ->
          %{size: size}
        {nil, precision, decimals, nil} ->
          %{precision: %{precision: precision, decimals: decimals}}
        {nil, nil, nil, with_timezone} ->
          %{withTimezone: with_timezone}
      end
    %{id: column.id,
      name: column.name,
      tableId: column.table_id,
      dataTypeId: column.data_type_id,
      modifier: modifier}
  end
end
