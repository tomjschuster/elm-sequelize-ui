defmodule SequelizeUi.DbDesign.Entity do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Entity, Schema, Field, Constraint}


  schema "entity" do
    field :name, :string
    field :schema_id, :integer

    belongs_to :schema, Schema, define_field: false
    has_many :fields, Field
    has_many :constraints, Constraint

    timestamps()
  end

  @doc false
  def changeset(%Entity{} = entity, attrs) do
    entity
    |> cast(attrs, [:name, :schema_id])
    |> validate_required([:name, :schema_id])
    |> assoc_constraint(:schema)
    |> unique_constraint(:name, name: :entity_schema_id_name_index)
  end
end
