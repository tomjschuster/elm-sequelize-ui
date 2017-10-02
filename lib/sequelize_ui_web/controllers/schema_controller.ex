defmodule SequelizeUiWeb.SchemaController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Schema

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    schemas = DbDesign.list_schemas()
    render(conn, "index.json", schemas: schemas)
  end

  def create(conn, %{"schema" => schema_params}) do
    with {:ok, %Schema{} = schema} <- DbDesign.create_schema(schema_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", schema_path(conn, :show, schema))
      |> render("show.json", schema: schema)
    end
  end

  def show(conn, %{"id" => id} = params) do
    if params["entities"] == "true" do
      schema = DbDesign.get_schema_with_entities!(id)
      render(conn, "show-with-entities.json", schema: schema)
    else
      schema = DbDesign.get_schema!(id)
      render(conn, "show.json", schema: schema)
    end
  end

  def update(conn, %{"id" => id, "schema" => schema_params}) do
    schema = DbDesign.get_schema!(id)
    with {:ok, %Schema{} = schema} <- DbDesign.update_schema(schema, schema_params) do
      render(conn, "show.json", schema: schema)
    end
  end

  def delete(conn, %{"id" => id}) do
    schema = DbDesign.get_schema!(id)
    with {:ok, %Schema{}} <- DbDesign.delete_schema(schema) do
      send_resp(conn, :no_content, "")
    end
  end
end
