defmodule SequelizeUi.DbDesign.Field do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Field, Entity}


  schema "field" do
    field :name, :string
    field :entity_id, :integer

    belongs_to :entity, Entity, define_field: false

    timestamps()
  end

  @doc false
  def changeset(%Field{} = field, attrs) do
    field
    |> cast(attrs, [:name, :entity_id])
    |> validate_required([:name, :entity_id])
    |> assoc_constraint(:entity)
    |> unique_constraint(:name, name: :field_entity_id_name_index)
  end
end
