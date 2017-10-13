defmodule SequelizeUiWeb.TableController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Table

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    tables = DbDesign.list_tables()
    render(conn, "index.json", tables: tables)
  end

  def create(conn, %{"table" => table_params}) do
    with {:ok, %Table{} = table} <- DbDesign.create_table(table_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", table_path(conn, :show, table))
      |> render("show.json", table: table)
    end
  end

  def show(conn, %{"id" => id} = params) do
    %{schema: with_schema, columns: with_columns} = conn.assigns.combined_with
    case {with_schema, with_columns} do
      {true, true} ->
        table = DbDesign.get_table_with_all!(id)
        render(conn, "show-with-all.json", table: table)
      {true, false} ->
        table = DbDesign.get_table_with_schema!(id)
        render(conn, "show-with-schema.json", table: table)
      {false, true} ->
        table = DbDesign.get_table_with_columns!(id)
        render(conn, "show-with-columns.json", table: table)
      {false, false} ->
        table = DbDesign.get_table!(id)
        render(conn, "show.json", table: table)
    end
  end

  def update(conn, %{"id" => id, "table" => table_params}) do
    table = DbDesign.get_table!(id)

    with {:ok, %Table{} = table} <- DbDesign.update_table(table, table_params) do
      render(conn, "show.json", table: table)
    end
  end

  def delete(conn, %{"id" => id}) do
    table = DbDesign.get_table!(id)
    with {:ok, %Table{}} <- DbDesign.delete_table(table) do
      send_resp(conn, :no_content, "")
    end
  end
end
