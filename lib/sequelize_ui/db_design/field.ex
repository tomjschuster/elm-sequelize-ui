defmodule SequelizeUi.DbDesign.Field do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Field, Entity, DataType}


  schema "field" do
    field :entity_id, :integer
    field :name, :string
    field :data_type_id, :integer
    field :size, :integer
    field :precision, :integer
    field :decimals, :integer
    field :with_timezone, :boolean

    belongs_to :entity, Entity, define_field: false
    belongs_to :data_type, DataType, define_field: false

    timestamps()
  end

  @doc false
  def changeset(%Field{} = field, attrs) do
    field
    |> cast(attrs, [:entity_id, :name, :data_type_id, :size, :precision, :decimals, :with_timezone])
    |> validate_required([:entity_id, :name, :data_type_id])
    |> assoc_constraint(:entity)
    |> assoc_constraint(:data_type)
    |> unique_constraint(:name, name: :field_entity_id_name_index)
  end
end
