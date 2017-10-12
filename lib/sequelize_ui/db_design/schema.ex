defmodule SequelizeUi.DbDesign.Schema do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Schema, Table}


  schema "schema" do
    field :name, :string

    has_many :tables, Table

    timestamps()
  end

  @doc false
  def changeset(%Schema{} = schema, attrs) do
    schema
    |> cast(attrs, [:name])
    |> validate_required([:name])
    |> unique_constraint(:name)
  end
end
