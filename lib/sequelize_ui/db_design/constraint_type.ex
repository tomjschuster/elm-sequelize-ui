defmodule SequelizeUi.DbDesign.ConstraintType do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{ConstraintType, Constraint}


  schema "constraint_type" do
    field :name, :string

    has_many :constraints, Constraint

    timestamps()
  end

  @doc false
  def changeset(%ConstraintType{} = constraint_type, attrs) do
    constraint_type
    |> cast(attrs, [:name])
    |> validate_required([:name])
    |> unique_constraint(:name)
  end
end
