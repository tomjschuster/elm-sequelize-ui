defmodule SequelizeUiWeb.ColumnController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.{Column, Table, Constraint}

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    columns = DbDesign.list_columns()
    render(conn, "index.json", columns: columns)
  end

  def for_table(conn, %{"table_id" => table_id}) do
    columns = DbDesign.list_columns_for_table(table_id)
    render(conn, "index.json", columns: columns)
  end

  def create(conn, params) do
    with {:ok, result} <- DbDesign.create_column_with_constraints(params),
         %{column: column, constraints: constraints} <- result do
      conn
      |> put_status(:created)
      |> put_resp_header("location", column_path(conn, :show, column))
      |> render("show-with-constraints.json", column: column, constraints: constraints)
    end
  end

  def show(conn, %{"id" => id} = params) do
    %{schema: with_schema, table: with_table} = conn.assigns.combined_with
    case {with_schema, with_table} do
      {true, true} ->
        column = DbDesign.get_column_with_all!(id)
        render(conn, "show-with-all.json", column: column)
      {false, true} ->
        column = DbDesign.get_column_with_table!(id)
        render(conn, "show-with-table.json", column: column)
      {false, false} ->
        column = DbDesign.get_column!(id)
        render(conn, "show.json", column: column)
    end
  end

  def update(conn, params) do
    with {:ok, result} <- DbDesign.update_column_with_constraints(params),
         %{column: column, constraints: constraints} <- result do
      conn
      |> put_status(:created)
      |> put_resp_header("location", column_path(conn, :show, column))
      |> render("show-with-constraints.json", column: column, constraints: constraints)
    end
  end

  def delete(conn, params) do
    with {:ok, result} <- DbDesign.delete_column_with_constraints(params) do
      send_resp(conn, :no_content, "")
    end
  end
end
