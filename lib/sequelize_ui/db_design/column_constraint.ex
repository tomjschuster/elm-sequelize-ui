defmodule SequelizeUi.DbDesign.ColumnConstraint do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.ColumnConstraint


  schema "column_constraint" do
    field :column_id, :id
    field :constraint_id, :id
    field :references_id, :id

    timestamps()
  end

  @doc false
  def changeset(%ColumnConstraint{} = column, attrs) do
    column
    |> cast(attrs, [:column_id, :constraint_id, :references_id])
    |> validate_required([:column_id, :constraint_id])
  end
end
