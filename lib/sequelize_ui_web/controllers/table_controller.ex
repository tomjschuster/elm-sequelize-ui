defmodule SequelizeUiWeb.TableController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Table

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    tables = DbDesign.list_tables()
    render(conn, "index.json", tables: tables)
  end

  def index_for_schema(conn, %{"schema_id" => schema_id, "data_type_id" => data_type_id} = params) do
    processed_params= process_data_type_params(params)
    tables = DbDesign.list_tables_for_schema_by_data_type(schema_id, processed_params)
    render(conn, "index.json", tables: tables)
  end

  def index_for_schema(conn, %{"schema_id" => schema_id} = params) do
    tables = DbDesign.list_tables_for_schema(schema_id)
    render(conn, "index.json", tables: tables)
  end



  def process_data_type_params(params) do
    processed_params =
      params
      |> Map.take(["data_type_id", "size", "precision", "scale", "with_timezone"])
      |> Enum.into(%{}, fn {k, v} -> {String.to_atom(k), v} end)

    case processed_params.data_type_id do
      "1" -> process_size_params(processed_params)
      "2" -> process_size_params(processed_params)
      "3" -> process_plain_params(processed_params)
      "4" -> process_size_params(processed_params)
      "5" -> process_size_params(processed_params)
      "6" -> process_plain_params(processed_params)
      "7" -> process_plain_params(processed_params)
      "8" -> process_plain_params(processed_params)
      "9" -> process_plain_params(processed_params)
      "10" -> process_plain_params(processed_params)
      "11" -> process_plain_params(processed_params)
      "12" -> process_precision_params(processed_params)
      "13" -> process_plain_params(processed_params)
      "14" -> process_plain_params(processed_params)
      "15" -> process_plain_params(processed_params)
      "16" -> process_plain_params(processed_params)
      "17" -> process_plain_params(processed_params)
      "18" -> process_timezone_params(processed_params)
      "19" -> process_timezone_params(processed_params)
    end
  end

  def process_plain_params(params) do
    [data_type_id: params.data_type_id]
  end

  def process_size_params(params) do
    [
      data_type_id: params.data_type_id,
      size: String.to_integer(params.size)
    ]
  end

  def process_precision_params(params) do
    [
      data_type_id: params.data_type_id,
      precision: String.to_integer(params.precision),
      scale: String.to_integer(params.scale)
    ]
  end

  def process_timezone_params(params) do
    [
      data_type_id: params.data_type_id,
      with_timezone: String.to_existing_atom(params.with_timezone)
    ]
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
