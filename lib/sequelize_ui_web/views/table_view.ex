defmodule SequelizeUiWeb.TableView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{TableView, ColumnView}

  def render("index.json", %{tables: tables}) do
    %{data: render_many(tables, TableView, "table.json")}
  end

  def render("index-candidates.json", %{tables: tables, columns: columns}) do
    %{data:
      %{
        tables: render_many(tables, TableView, "table.json"),
        columns: render_many(columns, ColumnView, "column.json")
      }
    }
  end

  def render("show.json", %{table: table}) do
    %{data: render_one(table, TableView, "table.json")}
  end

  def render("table.json", %{table: table}) do
    %{id: table.id,
      name: table.name,
      schemaId: table.schema_id}
  end
end
