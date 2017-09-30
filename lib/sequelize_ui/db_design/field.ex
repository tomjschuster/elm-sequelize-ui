defmodule SequelizeUi.DbDesign.Field do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Field, Entity}


  schema "field" do
    field :name, :string

    belongs_to :entity, Entity

    timestamps()
  end

  @doc false
  def changeset(%Field{} = field, attrs) do
    field
    |> cast(attrs, [:name, :entity_id])
    |> validate_required([:name, :entity_id])
    |> assoc_constraint(:entity)
  end
end
