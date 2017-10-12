defmodule SequelizeUi.DbDesign.Constraint do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Constraint, ConstraintType, Schema, Entity}


  schema "sql_constraint" do
    field :name, :string
    field :constraint_type_id, :id
    field :schema_id, :id
    field :entity_id, :id

    belongs_to :constraint_type, ConstraintType, define_field: false
    belongs_to :schema, Schema, define_field: false
    belongs_to :entity, Entity, define_field: false

    timestamps()
  end

  @doc false
  def changeset(%Constraint{} = constraint, attrs) do
    constraint
    |> cast(attrs, [:name, :constraint_type_id, :schema_id])
    |> validate_required([:name, :constraint_type_id, :schema_id])
    |> assoc_constraint(:constraint_type)
    |> assoc_constraint(:schema)
    |> assoc_constraint(:entity)
    |> unique_constraint(:name, name: :sql_constraint_schema_id_name_index)
  end
end
