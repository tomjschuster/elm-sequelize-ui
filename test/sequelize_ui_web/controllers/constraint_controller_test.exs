defmodule SequelizeUiWeb.ConstraintControllerTest do
  use SequelizeUiWeb.ConnCase

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Constraint

  @create_attrs %{name: "some name"}
  @update_attrs %{name: "some updated name"}
  @invalid_attrs %{name: nil}

  def fixture(:constraint) do
    {:ok, constraint} = DbDesign.create_constraint(@create_attrs)
    constraint
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all constraints", %{conn: conn} do
      conn = get conn, constraint_path(conn, :index)
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create constraint" do
    test "renders constraint when data is valid", %{conn: conn} do
      conn = post conn, constraint_path(conn, :create), constraint: @create_attrs
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get conn, constraint_path(conn, :show, id)
      assert json_response(conn, 200)["data"] == %{
        "id" => id,
        "name" => "some name"}
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post conn, constraint_path(conn, :create), constraint: @invalid_attrs
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update constraint" do
    setup [:create_constraint]

    test "renders constraint when data is valid", %{conn: conn, constraint: %Constraint{id: id} = constraint} do
      conn = put conn, constraint_path(conn, :update, constraint), constraint: @update_attrs
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get conn, constraint_path(conn, :show, id)
      assert json_response(conn, 200)["data"] == %{
        "id" => id,
        "name" => "some updated name"}
    end

    test "renders errors when data is invalid", %{conn: conn, constraint: constraint} do
      conn = put conn, constraint_path(conn, :update, constraint), constraint: @invalid_attrs
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete constraint" do
    setup [:create_constraint]

    test "deletes chosen constraint", %{conn: conn, constraint: constraint} do
      conn = delete conn, constraint_path(conn, :delete, constraint)
      assert response(conn, 204)
      assert_error_sent 404, fn ->
        get conn, constraint_path(conn, :show, constraint)
      end
    end
  end

  defp create_constraint(_) do
    constraint = fixture(:constraint)
    {:ok, constraint: constraint}
  end
end
