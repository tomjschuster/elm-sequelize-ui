defmodule SequelizeUiWeb.ColumnController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.{Column, Table, Constraint}

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    columns = DbDesign.list_columns()
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

  def update(conn, %{"id" => id, "column" => column_params, "constraints" => con_params}) do
    column = DbDesign.get_column!(id)
    table = DbDesign.get_table!(column.table_id)
    with {:ok, %Column{} = column} <- DbDesign.update_column(column, column_params),
         {:ok, count} <- DbDesign.delete_column_constraints(id),
         :ok <- DbDesign.create_column_constraints(table, column, con_params),
         all_constraints <- DbDesign.get_table_constraints(table.id) do
      render(conn, "show-with-constraints.json", column: column, constraints: all_constraints)
    end
  end

  def delete(conn, %{"id" => id}) do
    column = DbDesign.get_column!(id)
    with {:ok, count} <- DbDesign.delete_column_constraints(id),
         {:ok, %Column{}} <- DbDesign.delete_column(column) do
      send_resp(conn, :no_content, "")
    end
  end
end
