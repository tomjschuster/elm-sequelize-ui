defmodule SequelizeUiWeb.ColumnView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{ColumnView}
  alias SequelizeUi.DbDesign.DataType

  def render("index.json", %{columns: columns}) do
    %{data: render_many(columns, ColumnView, "column.json")}
  end

  def render("show.json", %{column: column}) do
    %{data: render_one(column, ColumnView, "column.json")}
  end

  def render("column.json", %{column: column}) do
    %{id: column.id,
      name: column.name,
      tableId: column.table_id,
      dataType: column.data_type.name,
      size: column.size,
      precision: column.precision,
      scale: column.scale,
      withTimezone: column.with_timezone}
  end
end
