defmodule SequelizeUi.DbDesign.DataType do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.DataType


  schema "data_type" do
    field :name, :string

    timestamps()
  end

  @doc false
  def changeset(%DataType{} = data_type, attrs) do
    data_type
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end
end
