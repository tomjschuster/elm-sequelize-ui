defmodule SequelizeUiWeb.ColumnController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    columns = DbDesign.list_columns()
    render(conn, "index.json", columns: columns)
  end

  def index_for_table(conn, %{"table_id" => schema_id, "data_type_id" => data_type_id} = params) do
    processed_params= SequelizeUiWeb.TableController.process_data_type_params(params)
    columns = DbDesign.list_columns_for_table_by_data_type(schema_id, processed_params)
    render(conn, "index.json", columns: columns)
  end

  def index_for_table(conn, %{"table_id" => table_id} = params) do
    data_type_id =
      case params |> Map.get("data_type_id", "") |> Integer.parse() do
        {data_type_id, ""} ->
          data_type_id
        otherwise ->
          nil
      end
    columns = DbDesign.list_columns_for_table(table_id, data_type_id)
    render(conn, "index.json", columns: columns)
  end

  def index_references(conn, %{"table_id" => table_id}) do
    columns = DbDesign.list_reference_columns_for_table(table_id)
    render(conn, "index.json", columns: columns)
  end

  def create(conn, params) do
    with {:ok, result} <- DbDesign.create_column_with_constraints(params),
         %{column: column} <- result do
      conn
      |> put_status(:created)
      |> put_resp_header("location", column_path(conn, :show, column))
      |> render("show.json", column: column)
    end
  end

  def show(conn, %{"id" => id}) do
    column = DbDesign.get_column!(id)
    render(conn, "show.json", column: column)
  end

  def update(conn, params) do
    with {:ok, result} <- DbDesign.update_column_with_constraints(params),
         %{column: column} <- result do
      conn
      |> put_status(:created)
      |> put_resp_header("location", column_path(conn, :show, column))
      |> render("show.json", column: column)
    end
  end

  def delete(conn, params) do
    with {:ok, _result} <- DbDesign.delete_column_with_constraints(params) do
      send_resp(conn, :no_content, "")
    end
  end
end
