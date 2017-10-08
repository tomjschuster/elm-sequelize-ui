defmodule SequelizeUi.DbDesign.DataType do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.DataType


  schema "data_type" do
    field :string_value, :string

    timestamps()
  end

  @doc false
  def changeset(%DataType{} = data_type, attrs) do
    data_type
    |> cast(attrs, [:string_value])
    |> validate_required([:string_value])
  end
end
