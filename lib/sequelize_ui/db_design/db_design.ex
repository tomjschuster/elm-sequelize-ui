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

      iex> create_schema(%{field: value})
      {:ok, %Schema{}}

      iex> create_schema(%{field: bad_value})
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

      iex> update_schema(schema, %{field: new_value})
      {:ok, %Schema{}}

      iex> update_schema(schema, %{field: bad_value})
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
  Gets a single table with its fields.

  Raises `Ecto.NoResultsError` if the Table does not exist.

  ## Examples

      iex> get_table_with_fields!(123)
      %Table{fields: [%Field{} | _rest]}

      iex> get_table_with_fields!(456)
      ** (Ecto.NoResultsError)
  """
  def get_table_with_fields!(id) do
    Repo.one! from e in Table,
      left_join: f in assoc(e, :fields),
      where: e.id == ^id,
      preload: [fields: f]
  end

  @doc """
  Gets a single table with its schema and fields.

  Raises `Ecto.NoResultsError` if the Table or Schema does not exist.

  ## Examples

      iex> get_table_with_all!(123)
      %Table{schema: %Schema{}, fields: [%Field{} | _rest]}

      iex> get_table_with_all!(456)
      ** (Ecto.NoResultsError)
  """
  def get_table_with_all!(id) do
    Repo.one! from e in Table,
      join: s in assoc(e, :schema),
      left_join: f in assoc(e, :fields),
      where: e.id == ^id,
      preload: [schema: s, fields: f]
  end

  @doc """
  Creates a table.

  ## Examples

      iex> create_table(%{field: value})
      {:ok, %Table{}}

      iex> create_table(%{field: bad_value})
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

      iex> update_table(table, %{field: new_value})
      {:ok, %Table{}}

      iex> update_table(table, %{field: bad_value})
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

  alias SequelizeUi.DbDesign.Field

  @doc """
  Returns the list of fields.

  ## Examples

      iex> list_fields()
      [%Field{}, ...]

  """
  def list_fields do
    Repo.all(Field)
  end

  @doc """
  Gets a single field.

  Raises `Ecto.NoResultsError` if the Field does not exist.

  ## Examples

      iex> get_field!(123)
      %Field{}

      iex> get_field!(456)
      ** (Ecto.NoResultsError)

  """
  def get_field!(id), do: Repo.get!(Field, id)

  @doc """
  Gets a single field with table parent.

  Raises `Ecto.NoResultsError` if the Field does not exist.

  ## Examples

      iex> get_field_with_table!(123)
      %Field{table: %Table{}}

      iex> get_field_with_table!(456)
      ** (Ecto.NoResultsError)

  """
  def get_field_with_table!(id) do
    Repo.one from f in Field,
      join: e in assoc(f, :table),
      where: f.id == ^id,
      preload: [table: e]
  end

  @doc """
  Gets a single field with table parent and schema grandparent.

  Raises `Ecto.NoResultsError` if the Field does not exist.

  ## Examples

      iex> get_field_with_all!(123)
      %Field{table: %Table{}}

      iex> get_field_with_all!(456)
      ** (Ecto.NoResultsError)

  """
  def get_field_with_all!(id) do
    Repo.one from f in Field,
      join: e in assoc(f, :table),
      join: s in assoc(e, :schema),
      where: f.id == ^id,
      preload: [table: {e, schema: s}]
  end

  @doc """
  Creates a field.

  ## Examples

      iex> create_field(%{field: value})
      {:ok, %Field{}}

      iex> create_field(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_field(attrs \\ %{}) do
    %Field{}
    |> Field.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a field.

  ## Examples

      iex> update_field(field, %{field: new_value})
      {:ok, %Field{}}

      iex> update_field(field, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_field(%Field{} = field, attrs) do
    field
    |> Field.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Field.

  ## Examples

      iex> delete_field(field)
      {:ok, %Field{}}

      iex> delete_field(field)
      {:error, %Ecto.Changeset{}}

  """
  def delete_field(%Field{} = field) do
    Repo.delete(field)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking field changes.

  ## Examples

      iex> change_field(field)
      %Ecto.Changeset{source: %Field{}}

  """
  def change_field(%Field{} = field) do
    Field.changeset(field, %{})
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

      iex> create_constraint(%{field: value})
      {:ok, %Constraint{}}

      iex> create_constraint(%{field: bad_value})
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

      iex> update_constraint(constraint, %{field: new_value})
      {:ok, %Constraint{}}

      iex> update_constraint(constraint, %{field: bad_value})
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

  alias SequelizeUi.DbDesign.Constraint

  @doc """
  Returns the list of constraint types.

  ## Examples

      iex> list_constraint_types()
      [%ConstraintType{}, ...]

  """
  def list_constraint_types do
    Repo.all(ConstraintType)
  end
end
