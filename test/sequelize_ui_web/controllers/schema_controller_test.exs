defmodule SequelizeUiWeb.SchemaControllerTest do
  use SequelizeUiWeb.ConnCase

  alias SequelizeUi.DbDesign
  alias SequelizeUi.DbDesign.Schema

  @create_attrs %{name: "some name"}
  @update_attrs %{name: "some updated name"}
  @invalid_attrs %{name: nil}

  def fixture(:schema) do
    {:ok, schema} = DbDesign.create_schema(@create_attrs)
    schema
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all schemas", %{conn: conn} do
      conn = get conn, schema_path(conn, :index)
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create schema" do
    test "renders schema when data is valid", %{conn: conn} do
      conn = post conn, schema_path(conn, :create), schema: @create_attrs
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get conn, schema_path(conn, :show, id)
      assert json_response(conn, 200)["data"] == %{
        "id" => id,
        "name" => "some name"}
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post conn, schema_path(conn, :create), schema: @invalid_attrs
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update schema" do
    setup [:create_schema]

    test "renders schema when data is valid", %{conn: conn, schema: %Schema{id: id} = schema} do
      conn = put conn, schema_path(conn, :update, schema), schema: @update_attrs
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get conn, schema_path(conn, :show, id)
      assert json_response(conn, 200)["data"] == %{
        "id" => id,
        "name" => "some updated name"}
    end

    test "renders errors when data is invalid", %{conn: conn, schema: schema} do
      conn = put conn, schema_path(conn, :update, schema), schema: @invalid_attrs
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete schema" do
    setup [:create_schema]

    test "deletes chosen schema", %{conn: conn, schema: schema} do
      conn = delete conn, schema_path(conn, :delete, schema)
      assert response(conn, 204)
      assert_error_sent 404, fn ->
        get conn, schema_path(conn, :show, schema)
      end
    end
  end

  defp create_schema(_) do
    schema = fixture(:schema)
    {:ok, schema: schema}
  end
end
