defmodule SequelizeUi.DbDesign.Schema do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Schema, Entity}


  schema "schema" do
    field :name, :string

    has_many :entities, Entity

    timestamps()
  end

  @doc false
  def changeset(%Schema{} = schema, attrs) do
    schema
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end