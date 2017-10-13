defmodule SequelizeUiWeb.FieldController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Field

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    fields = DbDesign.list_fields()
    render(conn, "index.json", fields: fields)
  end

  def create(conn, %{"field" => field_params}) do
    with {:ok, %Field{} = field} <- DbDesign.create_field(field_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", field_path(conn, :show, field))
      |> render("show.json", field: field)
    end
  end

  def show(conn, %{"id" => id} = params) do
    %{schema: with_schema, table: with_table} = conn.assigns.combined_with
    case {with_schema, with_table} do
      {true, true} ->
        field = DbDesign.get_field_with_all!(id)
        render(conn, "show-with-all.json", field: field)
      {false, true} ->
        field = DbDesign.get_field_with_table!(id)
        render(conn, "show-with-table.json", field: field)
      {false, false} ->
        field = DbDesign.get_field!(id)
        render(conn, "show.json", field: field)
    end
  end

  def update(conn, %{"id" => id, "field" => field_params}) do
    field = DbDesign.get_field!(id)
    with {:ok, %Field{} = field} <- DbDesign.update_field(field, field_params) do
      render(conn, "show.json", field: field)
    end
  end

  def delete(conn, %{"id" => id}) do
    field = DbDesign.get_field!(id)
    with {:ok, %Field{}} <- DbDesign.delete_field(field) do
      send_resp(conn, :no_content, "")
    end
  end
end
