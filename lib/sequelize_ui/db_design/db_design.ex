defmodule SequelizeUi.DbDesign do
  @moduledoc """
  The DbDesign context.
  """

  import Ecto.Query, warn: false
  alias Ecto.Multi
  alias SequelizeUi.Repo
  alias SequelizeUi.DbDesign.{
    Schema,
    Table,
    Column,
    Constraint,
    ConstraintType,
    ColumnConstraint
  }

  def list_schemas do
    Repo.all(Schema)
  end

  def get_schema!(id), do: Repo.get!(Schema, id)

  def get_schema_with_tables!(id) do
    Repo.one! from s in Schema,
      left_join: e in assoc(s, :tables),
      where: s.id == ^id,
      preload: [tables: e]
  end

  def create_schema(attrs \\ %{}) do
    %Schema{}
    |> Schema.changeset(attrs)
    |> Repo.insert()
  end

  def update_schema(%Schema{} = schema, attrs) do
    schema
    |> Schema.changeset(attrs)
    |> Repo.update()
  end

  def delete_schema(%Schema{} = schema) do
    Repo.delete(schema)
  end

  def change_schema(%Schema{} = schema) do
    Schema.changeset(schema, %{})
  end


  def list_tables do
    Repo.all(Table)
  end

  def get_table!(id), do: Repo.get!(Table, id)

  def get_table_with_schema!(id) do
    Repo.one! from e in Table,
      join: s in assoc(e, :schema),
      where: e.id == ^id,
      preload: [schema: s]
  end

  def get_table_with_columns!(id) do
    Repo.one! from e in Table,
      left_join: f in assoc(e, :columns),
      where: e.id == ^id,
      preload: [columns: f]
  end

  def get_table_with_all!(id) do
    Repo.one! from t in Table,
      join: s in assoc(t, :schema),
      left_join: col in assoc(t, :columns),
      left_join: con in assoc(t, :constraints),
      left_join: col_con in assoc(con, :column_constraints),
      left_join: con_type in assoc(con, :constraint_type),
      where: t.id == ^id,
      preload: [
        schema: s,
        columns: col,
        constraints: {con, [column_constraints: col_con, constraint_type: con_type]}
      ]
  end

  def create_table(attrs \\ %{}) do
    %Table{}
    |> Table.changeset(attrs)
    |> Repo.insert()
  end

  def update_table(%Table{} = table, attrs) do
    table
    |> Table.changeset(attrs)
    |> Repo.update()
  end

  def delete_table(%Table{} = table) do
    Repo.delete(table)
  end

  def change_table(%Table{} = table) do
    Table.changeset(table, %{})
  end

  def list_columns do
    Repo.all(Column)
  end

  def get_column!(id), do: Repo.get!(Column, id)

  def get_column_with_table!(id) do
    Repo.one from f in Column,
      join: e in assoc(f, :table),
      where: f.id == ^id,
      preload: [table: e]
  end

  def get_column_with_all!(id) do
    Repo.one from f in Column,
      join: e in assoc(f, :table),
      join: s in assoc(e, :schema),
      where: f.id == ^id,
      preload: [table: {e, schema: s}]
  end

  def create_column(attrs \\ %{}) do
    %Column{}
    |> Column.changeset(attrs)
    |> Repo.insert()
  end

  def update_column(%Column{} = column, attrs) do
    column
    |> Column.changeset(attrs)
    |> Repo.update()
  end

  def delete_column(%Column{} = column) do
    Repo.delete(column)
  end

  def change_column(%Column{} = column) do
    Column.changeset(column, %{})
  end


  def list_constraints do
    Repo.all(Constraint)
  end


  def create_constraint(attrs \\ %{}) do
    %Constraint{}
    |> Constraint.changeset(attrs)
    |> Repo.insert()
  end

  def update_constraint(%Constraint{} = constraint, attrs) do
    constraint
    |> Constraint.changeset(attrs)
    |> Repo.update()
  end


  def get_constraint_type_id(enum_name) do
    with %ConstraintType{id: id} <- Repo.get_by(ConstraintType, enum_name: enum_name),
      do: id
  end

  defp get_table_constraints(table_id) do
    Repo.all from con in Constraint,
      join: table in assoc(con, :table),
      join: col in assoc(con, :columns),
      join: col_con in assoc(con, :column_constraints),
      join: type in assoc(con, :constraint_type),
      where: con.table_id == ^table_id,
      preload: [table: table, columns: col, column_constraints: col_con, constraint_type: type]
  end

  def create_column_with_constraints(%{"column" => col_attrs, "constraints" => con_attrs}) do
    Multi.new
    |> Multi.insert(:column, Column.changeset(%Column{}, col_attrs))
    |> Multi.run(:table, &({:ok, get_table!(&1.column.table_id)}))
    |> Multi.run(:col_constraints, &(create_column_constraints(&1, con_attrs)))
    |> Multi.run(:constraints, &({:ok, get_table_constraints(&1.table.id)}))
    |> Repo.transaction
  end

  def update_column_with_constraints(%{"id" => id, "column" => col_attrs, "constraints" => con_attrs}) do
    column = get_column!(id)
    Multi.new
    |> Multi.run(:table, fn _ -> {:ok, get_table!(column.table_id)} end)
    |> Multi.update(:column, Column.changeset(column, col_attrs))
    |> Multi.run(:deleted_constraints, fn _ -> delete_column_constraints(id) end)
    |> Multi.run(:col_constraints, &(create_column_constraints(&1, con_attrs)))
    |> Multi.run(:constraints, &({:ok, get_table_constraints(&1.table.id)}))
    |> Repo.transaction
  end

  def delete_column_with_constraints(%{"id" => id}) do
    column = get_column!(id)
    Multi.new
    |> Multi.run(:deleted_constraints, fn _ -> delete_column_constraints(id) end)
    |> Multi.delete(:column, column)
    |> Repo.transaction
  end

  defp create_column_constraints(%{table: table, column: column}, params \\ %{}) do
    with {:ok, pk_constraint} <- create_column_pk(table, column, params),
         {:ok, nn_constraint} <- create_column_nn(table, column, params),
         {:ok, dv_constraint} <- create_column_dv(table, column, params),
         {:ok, uq_constraint} <- create_column_uq(table, column, params) do
      [pk_constraint, nn_constraint, dv_constraint, uq_constraint]
      |> Enum.filter(&(&1))
      |> (&({:ok, &1})).()
    end
  end

  defp create_column_pk(_table, _column, %{"is_primary_key" => false}), do: {:ok, nil}
  defp create_column_pk(%Table{} = table, %Column{} = column, _params) do
    case get_pks(table.id) do
      [pk | _rest] ->
        add_primary_key(column, pk)
      [] ->
        attrs = constraint_attrs(table, "primary_key")
        build_constraint(table, column, attrs)
    end
  end

  defp get_pks(table_id) do
    Repo.all from con in Constraint,
      join: type in assoc(con, :constraint_type),
      where: type.enum_name == "primary_key" and con.table_id == ^table_id
  end

  defp add_primary_key(%Column{} = column, %Constraint{} = constraint) do
    with col_con_attrs <- con_coll_attrs(column.id, constraint.id),
     {:ok, column_constraint} <- create_column_constraint(col_con_attrs),
     do: {:ok, constraint}
  end

  defp create_column_nn(_table, _column, %{"is_not_null" => false}), do: {:ok, nil}
  defp create_column_nn(%Table{} = table, %Column{} = column, _params) do
    attrs = constraint_attrs(table, "not_null")
    build_constraint(table, column, attrs)
  end

  defp create_column_dv(_table, _column, %{"default_value" => nil}), do: {:ok, nil}
  defp create_column_dv(%Table{} = table, %Column{} = column, params) do
    attrs = constraint_attrs(table, "default_value", params["default_value"])
    build_constraint(table, column, attrs)
  end

  defp create_column_uq(_table, _column, %{"is_unique" => false}), do: {:ok, nil}
  defp create_column_uq(%Table{} = table, %Column{} = column, _params) do
    attrs = constraint_attrs(table, "unique_key")
    build_constraint(table, column, attrs)
  end

 defp constraint_attrs(%Table{} = table, enum, value \\ nil) do
  %{
      constraint_type_id: get_constraint_type_id(enum),
      table_id: table.id,
      schema_id: table.schema_id,
      value: value
    }
  end

  defp build_constraint(%Table{} = table, %Column{} = column, attrs) do
    with {:ok, constraint} <- create_constraint(attrs),
         col_con_attrs <- con_coll_attrs(column.id, constraint.id),
         {:ok, column_constraint} <- create_column_constraint(col_con_attrs),
         do: {:ok, constraint}
  end

  defp con_coll_attrs(column_id, constraint_id) do
    %{column_id: column_id, constraint_id: constraint_id}
  end

  defp delete_column_constraints(column_id) do
    constraints = get_column_constraints(column_id)
    Enum.each(constraints, &(Repo.delete!(&1)))
    {:ok, length(constraints)}
  end

  defp get_column_constraints(column_id) do
    (Repo.all from con in Constraint,
      join: col in assoc(con, :columns),
      where: col.id == ^column_id,
      preload: [columns: col]
    ) |> Enum.filter(fn %Constraint{columns: columns} -> length(columns) <= 1 end)
  end

  defp create_column_constraint(attrs \\ %{}) do
    %ColumnConstraint{}
    |> ColumnConstraint.changeset(attrs)
    |> Repo.insert()
  end
end

