defmodule SequelizeUi.DbDesign.FieldConstraint do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.FieldConstraint


  schema "field_constraint" do
    field :field_id, :id
    field :constraint_id, :id

    timestamps()
  end

  @doc false
  def changeset(%FieldConstraint{} = field_constraint, attrs) do
    field_constraint
    |> cast(attrs, [])
    |> validate_required([])
  end
end
