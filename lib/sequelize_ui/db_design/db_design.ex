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
  Gets a single Schema with its entities.

  Raises `Ecto.NoResultsError` if the Schema does not exist.

  ## Examples

      iex> get_schema_with_entities!!(123)
      %Schema{entities: [%Entity{} | _rest]}

      iex> get_schema_with_entities!(456)
      ** (Ecto.NoResultsError)
  """
  def get_schema_with_entities!(id) do
    Repo.one! from s in Schema,
      left_join: e in assoc(s, :entities),
      where: s.id == ^id,
      preload: [entities: e]
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

  alias SequelizeUi.DbDesign.Entity

  @doc """
  Returns the list of entities.

  ## Examples

      iex> list_entities()
      [%Entity{}, ...]

  """
  def list_entities do
    Repo.all(Entity)
  end

  @doc """
  Gets a single entity.

  Raises `Ecto.NoResultsError` if the Entity does not exist.

  ## Examples

      iex> get_entity!(123)
      %Entity{}

      iex> get_entity!(456)
      ** (Ecto.NoResultsError)

  """
  def get_entity!(id), do: Repo.get!(Entity, id)

  @doc """
  Gets a single entity with its parent schema.

  Raises `Ecto.NoResultsError` if the Entity or Schema does not exist.

  ## Examples

      iex> get_entity_with_schema!(123)
      %Entity{schema: %Schema{}}

      iex> get_entity_with_schema!(456)
      ** (Ecto.NoResultsError)
  """
  def get_entity_with_schema!(id) do
    Repo.one! from e in Entity,
      join: s in assoc(e, :schema),
      where: e.id == ^id,
      preload: [schema: s]
  end

  @doc """
  Gets a single entity with its fields.

  Raises `Ecto.NoResultsError` if the Entity does not exist.

  ## Examples

      iex> get_entity_with_fields!(123)
      %Entity{fields: [%Field{} | _rest]}

      iex> get_entity_with_fields!(456)
      ** (Ecto.NoResultsError)
  """
  def get_entity_with_fields!(id) do
    Repo.one! from e in Entity,
      left_join: f in assoc(e, :fields),
      where: e.id == ^id,
      preload: [fields: f]
  end

  @doc """
  Gets a single entity with its schema and fields.

  Raises `Ecto.NoResultsError` if the Entity or Schema does not exist.

  ## Examples

      iex> get_entity_with_all!(123)
      %Entity{schema: %Schema{}, fields: [%Field{} | _rest]}

      iex> get_entity_with_all!(456)
      ** (Ecto.NoResultsError)
  """
  def get_entity_with_all!(id) do
    Repo.one! from e in Entity,
      join: s in assoc(e, :schema),
      left_join: f in assoc(e, :fields),
      where: e.id == ^id,
      preload: [schema: s, fields: f]
  end

  @doc """
  Creates a entity.

  ## Examples

      iex> create_entity(%{field: value})
      {:ok, %Entity{}}

      iex> create_entity(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_entity(attrs \\ %{}) do
    %Entity{}
    |> Entity.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a entity.

  ## Examples

      iex> update_entity(entity, %{field: new_value})
      {:ok, %Entity{}}

      iex> update_entity(entity, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_entity(%Entity{} = entity, attrs) do
    entity
    |> Entity.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Entity.

  ## Examples

      iex> delete_entity(entity)
      {:ok, %Entity{}}

      iex> delete_entity(entity)
      {:error, %Ecto.Changeset{}}

  """
  def delete_entity(%Entity{} = entity) do
    Repo.delete(entity)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking entity changes.

  ## Examples

      iex> change_entity(entity)
      %Ecto.Changeset{source: %Entity{}}

  """
  def change_entity(%Entity{} = entity) do
    Entity.changeset(entity, %{})
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
  Gets a single field with entity parent.

  Raises `Ecto.NoResultsError` if the Field does not exist.

  ## Examples

      iex> get_field_with_entity!(123)
      %Field{entity: %Entity{}}

      iex> get_field_with_entity!(456)
      ** (Ecto.NoResultsError)

  """
  def get_field_with_entity!(id) do
    Repo.one from f in Field,
      join: e in assoc(f, :entity),
      where: f.id == ^id,
      preload: [entity: e]
  end

  @doc """
  Gets a single field with entity parent and schema grandparent.

  Raises `Ecto.NoResultsError` if the Field does not exist.

  ## Examples

      iex> get_field_with_all!(123)
      %Field{entity: %Entity{}}

      iex> get_field_with_all!(456)
      ** (Ecto.NoResultsError)

  """
  def get_field_with_all!(id) do
    Repo.one from f in Field,
      join: e in assoc(f, :entity),
      join: s in assoc(e, :schema),
      where: f.id == ^id,
      preload: [entity: {e, schema: s}]
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
end
