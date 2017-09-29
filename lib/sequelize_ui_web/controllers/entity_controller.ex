defmodule SequelizeUiWeb.EntityController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Entity

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    entities = DbDesign.list_entities()
    render(conn, "index.json", entities: entities)
  end

  def create(conn, %{"entity" => entity_params}) do
    with {:ok, %Entity{} = entity} <- DbDesign.create_entity(entity_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", entity_path(conn, :show, entity))
      |> render("show.json", entity: entity)
    end
  end

  def show(conn, %{"id" => id, "schema" => "show", "fields" => "show"}) do
    entity = DbDesign.get_entity_with_all!(id)
    render(conn, "show-with-all.json", entity: entity)
  end

  def show(conn, %{"id" => id, "schema" => "show"}) do
    entity = DbDesign.get_entity_with_schema!(id)
    render(conn, "show-with-schema.json", entity: entity)
  end

  def show(conn, %{"id" => id, "fields" => "show"}) do
    entity = DbDesign.get_entity_with_fields!(id)
    render(conn, "show-with-fields.json", entity: entity)
  end

  def show(conn, %{"id" => id}) do
    entity = DbDesign.get_entity!(id)
    render(conn, "show.json", entity: entity)
  end

  def update(conn, %{"id" => id, "entity" => entity_params}) do
    entity = DbDesign.get_entity!(id)

    with {:ok, %Entity{} = entity} <- DbDesign.update_entity(entity, entity_params) do
      render(conn, "show.json", entity: entity)
    end
  end

  def delete(conn, %{"id" => id}) do
    entity = DbDesign.get_entity!(id)
    with {:ok, %Entity{}} <- DbDesign.delete_entity(entity) do
      send_resp(conn, :no_content, "")
    end
  end
end
