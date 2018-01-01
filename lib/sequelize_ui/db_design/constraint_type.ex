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

  def id_to_atom(id) do
    case id do
      1 -> :primary_key
      2 -> :not_null
      3 -> :default_value
      4 -> :unique_key
      5 -> :foreign_key
      6 -> :check
    end
  end

  def atom_to_id(atom) do
    case atom do
      :primary_key -> 1
      :not_null -> 2
      :default_value -> 3
      :unique_key -> 4
      :foreign_key -> 5
      :check -> 6
    end
  end
end
