defmodule SequelizeUiWeb.FieldView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{FieldView, TableView, SchemaView}

  def render("index.json", %{fields: fields}) do
    %{data: render_many(fields, FieldView, "field.json")}
  end

  def render("show.json", %{field: field}) do
    %{data: render_one(field, FieldView, "field.json")}
  end

  def render("show-with-table.json", %{field: field}) do
    %{data: %{field: render_one(field, FieldView, "field.json"),
              table: render_one(field.table, TableView, "table.json")}}
  end

  def render("show-with-all.json", %{field: field}) do
    %{data: %{field: render_one(field, FieldView, "field.json"),
              table: render_one(field.table, TableView, "table.json"),
              schema: render_one(field.table.schema, SchemaView, "schema.json")}}
  end

  def render("field.json", %{field: field}) do
    modifier =
      case {field.size, field.precision, field.decimals, field.with_timezone} do
        {nil, nil, nil, nil} ->
          nil
        {size, nil, nil, nil} ->
          %{size: size}
        {nil, precision, decimals, nil} ->
          %{precision: %{precision: precision, decimals: decimals}}
        {nil, nil, nil, with_timezone} ->
          %{withTimezone: with_timezone}
      end
    %{id: field.id,
      name: field.name,
      tableId: field.table_id,
      dataTypeId: field.data_type_id,
      modifier: modifier}
  end
end
