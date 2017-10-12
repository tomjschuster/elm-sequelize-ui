defmodule SequelizeUi.DbDesignTest do
  use SequelizeUi.DataCase

  alias SequelizeUi.DbDesign

  describe "schemas" do
    alias SequelizeUi.DbDesign.Schema

    @valid_attrs %{name: "some name"}
    @update_attrs %{name: "some updated name"}
    @invalid_attrs %{name: nil}

    def schema_fixture(attrs \\ %{}) do
      {:ok, schema} =
        attrs
        |> Enum.into(@valid_attrs)
        |> DbDesign.create_schema()

      schema
    end

    test "list_schemas/0 returns all schemas" do
      schema = schema_fixture()
      assert DbDesign.list_schemas() == [schema]
    end

    test "get_schema!/1 returns the schema with given id" do
      schema = schema_fixture()
      assert DbDesign.get_schema!(schema.id) == schema
    end

    test "create_schema/1 with valid data creates a schema" do
      assert {:ok, %Schema{} = schema} = DbDesign.create_schema(@valid_attrs)
      assert schema.name == "some name"
    end

    test "create_schema/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = DbDesign.create_schema(@invalid_attrs)
    end

    test "update_schema/2 with valid data updates the schema" do
      schema = schema_fixture()
      assert {:ok, schema} = DbDesign.update_schema(schema, @update_attrs)
      assert %Schema{} = schema
      assert schema.name == "some updated name"
    end

    test "update_schema/2 with invalid data returns error changeset" do
      schema = schema_fixture()
      assert {:error, %Ecto.Changeset{}} = DbDesign.update_schema(schema, @invalid_attrs)
      assert schema == DbDesign.get_schema!(schema.id)
    end

    test "delete_schema/1 deletes the schema" do
      schema = schema_fixture()
      assert {:ok, %Schema{}} = DbDesign.delete_schema(schema)
      assert_raise Ecto.NoResultsError, fn -> DbDesign.get_schema!(schema.id) end
    end

    test "change_schema/1 returns a schema changeset" do
      schema = schema_fixture()
      assert %Ecto.Changeset{} = DbDesign.change_schema(schema)
    end
  end

  describe "entities" do
    alias SequelizeUi.DbDesign.Entity

    @valid_attrs %{name: "some name"}
    @update_attrs %{name: "some updated name"}
    @invalid_attrs %{name: nil}

    def entity_fixture(attrs \\ %{}) do
      {:ok, entity} =
        attrs
        |> Enum.into(@valid_attrs)
        |> DbDesign.create_entity()

      entity
    end

    test "list_entities/0 returns all entities" do
      entity = entity_fixture()
      assert DbDesign.list_entities() == [entity]
    end

    test "get_entity!/1 returns the entity with given id" do
      entity = entity_fixture()
      assert DbDesign.get_entity!(entity.id) == entity
    end

    test "create_entity/1 with valid data creates a entity" do
      assert {:ok, %Entity{} = entity} = DbDesign.create_entity(@valid_attrs)
      assert entity.name == "some name"
    end

    test "create_entity/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = DbDesign.create_entity(@invalid_attrs)
    end

    test "update_entity/2 with valid data updates the entity" do
      entity = entity_fixture()
      assert {:ok, entity} = DbDesign.update_entity(entity, @update_attrs)
      assert %Entity{} = entity
      assert entity.name == "some updated name"
    end

    test "update_entity/2 with invalid data returns error changeset" do
      entity = entity_fixture()
      assert {:error, %Ecto.Changeset{}} = DbDesign.update_entity(entity, @invalid_attrs)
      assert entity == DbDesign.get_entity!(entity.id)
    end

    test "delete_entity/1 deletes the entity" do
      entity = entity_fixture()
      assert {:ok, %Entity{}} = DbDesign.delete_entity(entity)
      assert_raise Ecto.NoResultsError, fn -> DbDesign.get_entity!(entity.id) end
    end

    test "change_entity/1 returns a entity changeset" do
      entity = entity_fixture()
      assert %Ecto.Changeset{} = DbDesign.change_entity(entity)
    end
  end

  describe "fields" do
    alias SequelizeUi.DbDesign.Field

    @valid_attrs %{name: "some name"}
    @update_attrs %{name: "some updated name"}
    @invalid_attrs %{name: nil}

    def field_fixture(attrs \\ %{}) do
      {:ok, field} =
        attrs
        |> Enum.into(@valid_attrs)
        |> DbDesign.create_field()

      field
    end

    test "list_fields/0 returns all fields" do
      field = field_fixture()
      assert DbDesign.list_fields() == [field]
    end

    test "get_field!/1 returns the field with given id" do
      field = field_fixture()
      assert DbDesign.get_field!(field.id) == field
    end

    test "create_field/1 with valid data creates a field" do
      assert {:ok, %Field{} = field} = DbDesign.create_field(@valid_attrs)
      assert field.name == "some name"
    end

    test "create_field/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = DbDesign.create_field(@invalid_attrs)
    end

    test "update_field/2 with valid data updates the field" do
      field = field_fixture()
      assert {:ok, field} = DbDesign.update_field(field, @update_attrs)
      assert %Field{} = field
      assert field.name == "some updated name"
    end

    test "update_field/2 with invalid data returns error changeset" do
      field = field_fixture()
      assert {:error, %Ecto.Changeset{}} = DbDesign.update_field(field, @invalid_attrs)
      assert field == DbDesign.get_field!(field.id)
    end

    test "delete_field/1 deletes the field" do
      field = field_fixture()
      assert {:ok, %Field{}} = DbDesign.delete_field(field)
      assert_raise Ecto.NoResultsError, fn -> DbDesign.get_field!(field.id) end
    end

    test "change_field/1 returns a field changeset" do
      field = field_fixture()
      assert %Ecto.Changeset{} = DbDesign.change_field(field)
    end
  end

  describe "constraints" do
    alias SequelizeUi.DbDesign.Constraint

    @valid_attrs %{name: "some name"}
    @update_attrs %{name: "some updated name"}
    @invalid_attrs %{name: nil}

    def constraint_fixture(attrs \\ %{}) do
      {:ok, constraint} =
        attrs
        |> Enum.into(@valid_attrs)
        |> DbDesign.create_constraint()

      constraint
    end

    test "list_constraints/0 returns all constraints" do
      constraint = constraint_fixture()
      assert DbDesign.list_constraints() == [constraint]
    end

    test "get_constraint!/1 returns the constraint with given id" do
      constraint = constraint_fixture()
      assert DbDesign.get_constraint!(constraint.id) == constraint
    end

    test "create_constraint/1 with valid data creates a constraint" do
      assert {:ok, %Constraint{} = constraint} = DbDesign.create_constraint(@valid_attrs)
      assert constraint.name == "some name"
    end

    test "create_constraint/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = DbDesign.create_constraint(@invalid_attrs)
    end

    test "update_constraint/2 with valid data updates the constraint" do
      constraint = constraint_fixture()
      assert {:ok, constraint} = DbDesign.update_constraint(constraint, @update_attrs)
      assert %Constraint{} = constraint
      assert constraint.name == "some updated name"
    end

    test "update_constraint/2 with invalid data returns error changeset" do
      constraint = constraint_fixture()
      assert {:error, %Ecto.Changeset{}} = DbDesign.update_constraint(constraint, @invalid_attrs)
      assert constraint == DbDesign.get_constraint!(constraint.id)
    end

    test "delete_constraint/1 deletes the constraint" do
      constraint = constraint_fixture()
      assert {:ok, %Constraint{}} = DbDesign.delete_constraint(constraint)
      assert_raise Ecto.NoResultsError, fn -> DbDesign.get_constraint!(constraint.id) end
    end

    test "change_constraint/1 returns a constraint changeset" do
      constraint = constraint_fixture()
      assert %Ecto.Changeset{} = DbDesign.change_constraint(constraint)
    end
  end
end
