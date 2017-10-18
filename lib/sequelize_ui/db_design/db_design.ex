defmodule SequelizeUi.DbDesign do
  @moduledoc """
  The DbDesign context.
  """

  import Ecto.Query, warn: false
  alias SequelizeUi.Repo

  alias SequelizeUi.DbDesign.Schema

  @doc """
  Returns the list of schemas.

  ## Examples

      iex> list_schemas()
      [%Schema{}, ...]

  """
  def list_schemas do
    Repo.all(Schema)
  end

  @doc """
  Gets a single schema.

  Raises `Ecto.NoResultsError` if the Schema does not exist.

  ## Examples

      iex> get_schema!(123)
      %Schema{}

      iex> get_schema!(456)
      ** (Ecto.NoResultsError)

  """
  def get_schema!(id), do: Repo.get!(Schema, id)

  @doc """
  Gets a single Schema with its tables.

  Raises `Ecto.NoResultsError` if the Schema does not exist.

  ## Examples

      iex> get_schema_with_tables!!(123)
      %Schema{tables: [%Table{} | _rest]}

      iex> get_schema_with_tables!(456)
      ** (Ecto.NoResultsError)
  """
  def get_schema_with_tables!(id) do
    Repo.one! from s in Schema,
      left_join: e in assoc(s, :tables),
      where: s.id == ^id,
      preload: [tables: e]
  end

  @doc """
  Creates a schema.

  ## Examples

      iex> create_schema(%{column: value})
      {:ok, %Schema{}}

      iex> create_schema(%{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_schema(attrs \\ %{}) do
    %Schema{}
    |> Schema.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a schema.

  ## Examples

      iex> update_schema(schema, %{column: new_value})
      {:ok, %Schema{}}

      iex> update_schema(schema, %{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_schema(%Schema{} = schema, attrs) do
    schema
    |> Schema.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Schema.

  ## Examples

      iex> delete_schema(schema)
      {:ok, %Schema{}}

      iex> delete_schema(schema)
      {:error, %Ecto.Changeset{}}

  """
  def delete_schema(%Schema{} = schema) do
    Repo.delete(schema)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking schema changes.

  ## Examples

      iex> change_schema(schema)
      %Ecto.Changeset{source: %Schema{}}

  """
  def change_schema(%Schema{} = schema) do
    Schema.changeset(schema, %{})
  end

  alias SequelizeUi.DbDesign.Table

  @doc """
  Returns the list of tables.

  ## Examples

      iex> list_tables()
      [%Table{}, ...]

  """
  def list_tables do
    Repo.all(Table)
  end

  @doc """
  Gets a single table.

  Raises `Ecto.NoResultsError` if the Table does not exist.

  ## Examples

      iex> get_table!(123)
      %Table{}

      iex> get_table!(456)
      ** (Ecto.NoResultsError)

  """
  def get_table!(id), do: Repo.get!(Table, id)

  @doc """
  Gets a single table with its parent schema.

  Raises `Ecto.NoResultsError` if the Table or Schema does not exist.

  ## Examples

      iex> get_table_with_schema!(123)
      %Table{schema: %Schema{}}

      iex> get_table_with_schema!(456)
      ** (Ecto.NoResultsError)
  """
  def get_table_with_schema!(id) do
    Repo.one! from e in Table,
      join: s in assoc(e, :schema),
      where: e.id == ^id,
      preload: [schema: s]
  end

  @doc """
  Gets a single table with its columns.

  Raises `Ecto.NoResultsError` if the Table does not exist.

  ## Examples

      iex> get_table_with_columns!(123)
      %Table{columns: [%Column{} | _rest]}

      iex> get_table_with_columns!(456)
      ** (Ecto.NoResultsError)
  """
  def get_table_with_columns!(id) do
    Repo.one! from e in Table,
      left_join: f in assoc(e, :columns),
      where: e.id == ^id,
      preload: [columns: f]
  end

  @doc """
  Gets a single table with its schema and columns.

  Raises `Ecto.NoResultsError` if the Table or Schema does not exist.

  ## Examples

      iex> get_table_with_all!(123)
      %Table{schema: %Schema{}, columns: [%Column{} | _rest]}

      iex> get_table_with_all!(456)
      ** (Ecto.NoResultsError)
  """
  def get_table_with_all!(id) do
    Repo.one! from e in Table,
      join: s in assoc(e, :schema),
      left_join: f in assoc(e, :columns),
      where: e.id == ^id,
      preload: [schema: s, columns: f]
  end

  @doc """
  Creates a table.

  ## Examples

      iex> create_table(%{column: value})
      {:ok, %Table{}}

      iex> create_table(%{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_table(attrs \\ %{}) do
    %Table{}
    |> Table.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a table.

  ## Examples

      iex> update_table(table, %{column: new_value})
      {:ok, %Table{}}

      iex> update_table(table, %{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_table(%Table{} = table, attrs) do
    table
    |> Table.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Table.

  ## Examples

      iex> delete_table(table)
      {:ok, %Table{}}

      iex> delete_table(table)
      {:error, %Ecto.Changeset{}}

  """
  def delete_table(%Table{} = table) do
    Repo.delete(table)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking table changes.

  ## Examples

      iex> change_table(table)
      %Ecto.Changeset{source: %Table{}}

  """
  def change_table(%Table{} = table) do
    Table.changeset(table, %{})
  end

  alias SequelizeUi.DbDesign.Column

  @doc """
  Returns the list of columns.

  ## Examples

      iex> list_columns()
      [%Column{}, ...]

  """
  def list_columns do
    Repo.all(Column)
  end

  @doc """
  Gets a single column.

  Raises `Ecto.NoResultsError` if the Column does not exist.

  ## Examples

      iex> get_column!(123)
      %Column{}

      iex> get_column!(456)
      ** (Ecto.NoResultsError)

  """
  def get_column!(id), do: Repo.get!(Column, id)

  @doc """
  Gets a single column with table parent.

  Raises `Ecto.NoResultsError` if the Column does not exist.

  ## Examples

      iex> get_column_with_table!(123)
      %Column{table: %Table{}}

      iex> get_column_with_table!(456)
      ** (Ecto.NoResultsError)

  """
  def get_column_with_table!(id) do
    Repo.one from f in Column,
      join: e in assoc(f, :table),
      where: f.id == ^id,
      preload: [table: e]
  end

  @doc """
  Gets a single column with table parent and schema grandparent.

  Raises `Ecto.NoResultsError` if the Column does not exist.

  ## Examples

      iex> get_column_with_all!(123)
      %Column{table: %Table{}}

      iex> get_column_with_all!(456)
      ** (Ecto.NoResultsError)

  """
  def get_column_with_all!(id) do
    Repo.one from f in Column,
      join: e in assoc(f, :table),
      join: s in assoc(e, :schema),
      where: f.id == ^id,
      preload: [table: {e, schema: s}]
  end

  @doc """
  Creates a column.

  ## Examples

      iex> create_column(%{column: value})
      {:ok, %Column{}}

      iex> create_column(%{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_column(attrs \\ %{}) do
    %Column{}
    |> Column.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a column.

  ## Examples

      iex> update_column(column, %{column: new_value})
      {:ok, %Column{}}

      iex> update_column(column, %{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_column(%Column{} = column, attrs) do
    column
    |> Column.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Column.

  ## Examples

      iex> delete_column(column)
      {:ok, %Column{}}

      iex> delete_column(column)
      {:error, %Ecto.Changeset{}}

  """
  def delete_column(%Column{} = column) do
    Repo.delete(column)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking column changes.

  ## Examples

      iex> change_column(column)
      %Ecto.Changeset{source: %Column{}}

  """
  def change_column(%Column{} = column) do
    Column.changeset(column, %{})
  end

  alias SequelizeUi.DbDesign.Constraint

  @doc """
  Returns the list of constraints.

  ## Examples

      iex> list_constraints()
      [%Constraint{}, ...]

  """
  def list_constraints do
    Repo.all(Constraint)
  end

  @doc """
  Gets a single constraint.

  Raises `Ecto.NoResultsError` if the Constraint does not exist.

  ## Examples

      iex> get_constraint!(123)
      %Constraint{}

      iex> get_constraint!(456)
      ** (Ecto.NoResultsError)

  """
  def get_constraint!(id), do: Repo.get!(Constraint, id)

  @doc """
  Creates a constraint.

  ## Examples

      iex> create_constraint(%{column: value})
      {:ok, %Constraint{}}

      iex> create_constraint(%{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_constraint(attrs \\ %{}) do
    %Constraint{}
    |> Constraint.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a constraint.

  ## Examples

      iex> update_constraint(constraint, %{column: new_value})
      {:ok, %Constraint{}}

      iex> update_constraint(constraint, %{column: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_constraint(%Constraint{} = constraint, attrs) do
    constraint
    |> Constraint.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Constraint.

  ## Examples

      iex> delete_constraint(constraint)
      {:ok, %Constraint{}}

      iex> delete_constraint(constraint)
      {:error, %Ecto.Changeset{}}

  """
  def delete_constraint(%Constraint{} = constraint) do
    Repo.delete(constraint)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking constraint changes.

  ## Examples

      iex> change_constraint(constraint)
      %Ecto.Changeset{source: %Constraint{}}

  """
  def change_constraint(%Constraint{} = constraint) do
    Constraint.changeset(constraint, %{})
  end

  alias SequelizeUi.DbDesign.ConstraintType

  def get_constraint_type_id(enum_name) do
    with %ConstraintType{id: id} <- Repo.get_by(ConstraintType, enum_name: enum_name),
      do: id
  end

  def get_table_constraints(table_id) do
    Repo.all from con in Constraint,
      join: table in assoc(con, :table),
      join: col in assoc(con, :columns),
      join: col_con in assoc(con, :column_constraints),
      join: type in assoc(con, :constraint_type),
      where: con.table_id == ^table_id,
      preload: [table: table, columns: col, column_constraints: col_con, constraint_type: type]
  end

  def create_column_constraints(%Table{} = table, %Column{} = column, params \\ %{}) do
    with {:ok, pk_constraint} <- create_column_pk(table, column, params),
         {:ok, nn_constraint} <- create_column_nn(table, column, params),
         {:ok, dv_constraint} <- create_column_dv(table, column, params),
         {:ok, uq_constraint} <- create_column_uq(table, column, params),
         do: :ok
  end

  defp create_column_pk(_table, _column, %{"is_primary_key" => false}), do: {:ok, nil}
  defp create_column_pk(%Table{} = table, %Column{} = column, _params) do
    attrs = constraint_attrs(table, "primary_key")
    build_constraint(table, column, attrs)
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
  def get_column_constraints(column_id) do
    Repo.all from con in Constraint,
      join: col in assoc(con, :columns),
      join: col_con in assoc(con, :column_constraints),
      group_by: col_con.constraint_id,
      where: col.id == ^column_id and count(con.id) == 1
  end

  def delete_column_constraints(column_id) do
    Repo.delete_all from con in Constraint,
      join: col in assoc(con, :columns),
      join: col_con in assoc(con, :column_constraints),
      group_by: col_con.constraint_id,
      where: col.id == ^column_id and count(con.id) == 1
  end

  @doc """
  Creates all column constraints described in `params` and returns list of
  `%Constraint{}`.

  ## Example

      iex> with %Column{} = column <- get_column(1),
      ...>      %{"is_primary_}
      iex> build_column_constraints
  """
  # def build_column_constraints(schema_id, %Column{} = column, params \\ %{}) do

  # end

  # def constraint_types_from_params(params \\ %{}) do
  #   %{
  #     "is_primary_key" => primary_key?,
  #     "is_not_null" => not_null?,
  #     "default_value" => default_value,
  #     "is_unique" => unique?
  #     } = params

  #   pk_id = pk_from_params(params)
  #   nn_id = nn_from_params(params)
  #   dv_id = dv_rom_params(params)
  #   uq_id = uq_from_params

  #   constraints =
  #     [{pk_from_params(params)}]
  #   Enum.filter([{pk_id, nil}, {nn_id, nil}, {dv_id, default_value}, {uq_id], &(&1))
  # end

  # defp pk_from_params(params \\ %{}), do: Map.get(params, "primary_key")
  # defp nn_from_params(params \\ %{}), do: Map.get(params, "not_null")
  # defp dv_from_params(params \\ %{}), do: Map.get(params, "default_value")
  # defp uq_from_params(params \\ %{}), do: Map.get(params, "unique_key")
  alias SequelizeUi.DbDesign.ColumnConstraint

  def create_column_constraint(attrs \\ %{}) do
    %ColumnConstraint{}
    |> ColumnConstraint.changeset(attrs)
    |> Repo.insert()
  end
end

