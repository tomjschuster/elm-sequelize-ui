defmodule SequelizeUi.DbDesign do
  @moduledoc """
  The DbDesign context.
  """

  import Ecto.Query, warn: false
  alias Ecto.{Multi, Changeset}
  alias SequelizeUi.Repo
  alias SequelizeUi.DbDesign.{
    Schema,
    Table,
    Column,
    Constraint,
    ConstraintType,
    ColumnConstraint
  }

  ## Schema

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

  ## Table

  def list_tables do
    Repo.all(Table)
  end

  def list_tables_for_schema(schema_id) do
    Repo.all(from Table, where: [schema_id: ^schema_id])
  end
  def list_tables_for_schema(schema_id, nil), do: list_tables_for_schema(schema_id)
  def list_tables_for_schema(schema_id, data_type_id) do
    Repo.all from t in Table,
      join: c in assoc(t, :columns),
      where: t.schema_id == ^schema_id and c.data_type_id == ^data_type_id
  end

  def list_tables_for_schema_by_data_type(schema_id, data_type_params) do
    column_query = from Column, where: ^data_type_params
    Repo.all from t in Table,
      join: c in ^column_query, on: [table_id: t.id],
      where: t.schema_id == ^schema_id,
      distinct: true
  end

  def list_reference_tables_for_table(table_id) do
    Repo.all from reference_table in Table,
      join: col in assoc(reference_table, :columns),
      join: con in assoc(col, :reference_constraints),
      join: source_table in assoc(con, :table),
      where: source_table.id == ^table_id
  end

  def get_table!(id), do: Repo.get!(Table, id)

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

  def list_columns_for_table(table_id) do
    Repo.all(from Column, where: [table_id: ^table_id])
  end
  def list_columns_for_table(table_id, nil), do: list_columns_for_table(table_id)
  def list_columns_for_table(table_id, data_type_id) do
    Repo.all from Column,
      where: [table_id: ^table_id, data_type_id: ^data_type_id]
  end

  def list_columns_for_schema(schema_id) do
    Repo.all from column in Column,
      join: table in assoc(column, :table),
      where: table.schema_id == ^schema_id
  end

  def list_columns_for_table_by_data_type(table_id, data_type_params) do
    column_query = from Column, where: ^data_type_params
    Repo.all from c in column_query,
      where: c.table_id == ^table_id,
      distinct: true
  end
  def list_columns_for_schema_by_data_type(schema_id, data_type_params) do
    column_query = from Column, where: ^data_type_params
    Repo.all from c in column_query,
      join: t in assoc(c, :table),
      where: t.schema_id == ^schema_id,
      distinct: true
  end

  def list_reference_columns_for_table(table_id) do
    Repo.all from reference_column in Column,
      join: constraint in assoc(reference_column, :reference_constraints),
      join: reference_table in assoc(reference_column, :table),
      join: source_table in assoc(constraint, :table),
      where: source_table.id == ^table_id
  end

  def get_column!(id), do: Repo.get!(Column, id)

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

  ## Constraint

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
    |> Repo.transaction
  end

  def update_column_with_constraints(%{"id" => id, "column" => col_attrs, "constraints" => con_attrs}) do
    column = get_column!(id)
    Multi.new
    |> Multi.run(:table, fn _ -> {:ok, get_table!(column.table_id)} end)
    |> Multi.update(:column, Column.changeset(column, col_attrs))
    |> Multi.run(:deleted_constraints, fn _ -> delete_column_constraints(id) end)
    |> Multi.run(:col_constraints, &(create_column_constraints(&1, con_attrs)))
    |> Repo.transaction
  end

  def delete_column_with_constraints(%{"id" => id}) do
    column = get_column!(id)
    Multi.new
    |> Multi.run(:deleted_constraints, fn _ -> delete_column_constraints(id) end)
    |> Multi.delete(:column, column)
    |> Repo.transaction
  end

  defp create_column_constraints(%{table: table, column: column}, params) do
    with {:ok, pk_constraint} <- create_column_pk(table, column, params),
         {:ok, nn_constraint} <- create_column_nn(table, column, params),
         {:ok, dv_constraint} <- create_column_dv(table, column, params),
         {:ok, uq_constraint} <- create_column_uq(table, column, params),
         {:ok, fk_constraint} <- create_column_fk(table, column, params) do
      [pk_constraint, nn_constraint, dv_constraint, uq_constraint, fk_constraint]
      |> Enum.filter(&(&1))
      |> (&({:ok, &1})).()
    end
  end

  defp create_column_pk(_table, _column, %{"is_primary_key" => false}), do: {:ok, nil}
  defp create_column_pk(%Table{} = table, %Column{} = column, _params) do
    pk = get_pk(table.id) || %Constraint{columns: []}
    attrs = constraint_attrs(table, "primary_key")

    pk
    |> Constraint.changeset(attrs)
    |> Changeset.put_assoc(:columns, [column | pk.columns])
    |> Repo.insert_or_update()
  end

  defp get_pk(table_id) do
    Repo.one from con in Constraint,
      join: type in assoc(con, :constraint_type),
      where: type.enum_name == "primary_key" and con.table_id == ^table_id,
      preload: [:columns]
  end

  defp add_primary_key(%Column{} = column, %Constraint{} = constraint) do
    with col_con_attrs <- make_col_con_attrs(column.id, nil, constraint.id),
     {:ok, _column_constraint} <- create_column_constraint(col_con_attrs),
     do: {:ok, constraint}
  end

  defp create_column_nn(_table, _column, %{"is_not_null" => false}), do: {:ok, nil}
  defp create_column_nn(%Table{} = table, %Column{} = column, _params) do
    attrs = constraint_attrs(table, "not_null")
    build_constraint(column, attrs)
  end

  defp create_column_dv(_table, _column, %{"default_value" => nil}), do: {:ok, nil}
  defp create_column_dv(%Table{} = table, %Column{} = column, params) do
    attrs = constraint_attrs(table, "default_value", params["default_value"])
    build_constraint(column, attrs)
  end

  defp create_column_uq(_table, _column, %{"is_unique" => false}), do: {:ok, nil}
  defp create_column_uq(%Table{} = table, %Column{} = column, _params) do
    attrs = constraint_attrs(table, "unique_key")
    build_constraint(column, attrs)
  end

  defp create_column_fk(_table, _column, %{"reference" => nil}), do: {:ok, nil}
  defp create_column_fk(%Table{} = table, %Column{} = column, params) do
    attrs = constraint_attrs(table, "foreign_key")
    build_constraint(column, attrs, Map.get(params, "reference"))
  end

 defp constraint_attrs(%Table{} = table, enum, value \\ nil) do
  %{
      constraint_type_id: get_constraint_type_id(enum),
      table_id: table.id,
      schema_id: table.schema_id,
      value: value
    }
  end

  defp build_constraint(%Column{} = column, attrs, reference_id \\ nil) do
    with {:ok, constraint} <- create_constraint(attrs),
         col_con_attrs <- make_col_con_attrs(column.id, reference_id, constraint.id),
         {:ok, _column_constraint} <- create_column_constraint(col_con_attrs),
         do: {:ok, constraint}
  end

  defp make_col_con_attrs(column_id, references_id, constraint_id) do
    %{
      column_id: column_id,
      references_id: references_id,
      constraint_id: constraint_id
    }
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

  defp create_column_constraint(attrs) do
    %ColumnConstraint{}
    |> ColumnConstraint.changeset(attrs)
    |> Repo.insert()
  end

  def list_constraints_for_table(table_id) do
    Repo.all from con in Constraint,
      join: col_con in assoc(con, :column_constraints),
      join: type in assoc(con, :constraint_type),
      where: [table_id: ^table_id],
      preload: [column_constraints: col_con, constraint_type: type]
  end

  def get_constraint!(id), do: Repo.get!(Constraint, id)

  def delete_constraint(%Constraint{} = constraint) do
    Repo.delete(constraint)
  end

  defp data_type_params_to_atoms(params) do
    params
    |> Map.take(["data_type_id", "size", "precision", "scale", "with_timezone"])
    |> Enum.into(%{}, fn {k, v} -> {String.to_atom(k), v} end)
    |> Map.update!(:data_type_id, &(String.to_integer(&1)))
  end

  def process_data_type_params(params) do
    params
    |> data_type_params_to_atoms()
    |> data_type_id_to_attrs()
  end

  defp data_type_id_to_attrs(params) do
    case params.data_type_id do
      1 -> size_params(params)
      2 -> size_params(params)
      3 -> plain_data_type_params(params)
      4 -> size_params(params)
      5 -> size_params(params)
      6 -> plain_data_type_params(params)
      7 -> plain_data_type_params(params)
      8 -> plain_data_type_params(params)
      9 -> plain_data_type_params(params)
      10 -> plain_data_type_params(params)
      11 -> plain_data_type_params(params)
      12 -> precision_params(params)
      13 -> plain_data_type_params(params)
      14 -> plain_data_type_params(params)
      15 -> plain_data_type_params(params)
      16 -> plain_data_type_params(params)
      17 -> plain_data_type_params(params)
      18 -> timezone_params(params)
      19 -> timezone_params(params)
    end
  end

  defp plain_data_type_params(params) do
    [data_type_id: params.data_type_id]
  end

  defp size_params(params) do
    [
      data_type_id: params.data_type_id,
      size: String.to_integer(params.size)
    ]
  end

  defp precision_params(params) do
    [
      data_type_id: params.data_type_id,
      precision: String.to_integer(params.precision),
      scale: String.to_integer(params.scale)
    ]
  end

  defp timezone_params(params) do
    [
      data_type_id: params.data_type_id,
      with_timezone: String.to_existing_atom(params.with_timezone)
    ]
  end
end

