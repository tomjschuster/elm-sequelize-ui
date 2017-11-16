defmodule SequelizeUiWeb.TableController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Table

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    tables = DbDesign.list_tables()
    render(conn, "index.json", tables: tables)
  end

  def index_candidates(conn, %{"schema_id" => schema_id, "data_type_id" => data_type_id} = params) do
    processed_params= DbDesign.process_data_type_params(params)
    tables = DbDesign.list_tables_for_schema_by_data_type(schema_id, processed_params)
    columns = DbDesign.list_columns_for_schema_by_data_type(schema_id, processed_params)
    render(conn, "index-candidates.json", tables: tables, columns: columns)
  end

  def index_for_schema(conn, %{"schema_id" => schema_id} = params) do
    tables = DbDesign.list_tables_for_schema(schema_id)
    render(conn, "index.json", tables: tables)
  end

  def index_references(conn, %{"table_id" => table_id}) do
    tables = DbDesign.list_reference_tables_for_table(table_id)
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

  def show(conn, %{"id" => id}) do
    table = DbDesign.get_table!(id)
    render(conn, "show.json", table: table)
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
