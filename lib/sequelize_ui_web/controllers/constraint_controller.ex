defmodule SequelizeUiWeb.ConstraintController do
  use SequelizeUiWeb, :controller

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Constraint

  action_fallback SequelizeUiWeb.FallbackController

  def index(conn, _params) do
    constraints = DbDesign.list_constraints()
    render(conn, "index.json", constraints: constraints)
  end

  def index_for_table(conn, %{"table_id" => table_id}) do
    constraints = DbDesign.list_constraints_for_table(table_id)
    render(conn, "index-for-table.json", constraints: constraints)
  end

  def create(conn, %{"constraint" => constraint_params}) do
    with {:ok, %Constraint{} = constraint} <- DbDesign.create_constraint(constraint_params) do
      conn
      |> put_status(:created)
      |> put_resp_header("location", constraint_path(conn, :show, constraint))
      |> render("show.json", constraint: constraint)
    end
  end

  def show(conn, %{"id" => id}) do
    constraint = DbDesign.get_constraint!(id)
    render(conn, "show.json", constraint: constraint)
  end

  def update(conn, %{"id" => id, "constraint" => constraint_params}) do
    constraint = DbDesign.get_constraint!(id)

    with {:ok, %Constraint{} = constraint} <- DbDesign.update_constraint(constraint, constraint_params) do
      render(conn, "show.json", constraint: constraint)
    end
  end

  def delete(conn, %{"id" => id}) do
    constraint = DbDesign.get_constraint!(id)
    with {:ok, %Constraint{}} <- DbDesign.delete_constraint(constraint) do
      send_resp(conn, :no_content, "")
    end
  end
end
