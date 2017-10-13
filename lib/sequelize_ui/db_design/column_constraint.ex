defmodule SequelizeUi.DbDesign.ColumnConstraint do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.ColumnConstraint


  schema "sql_column" do
    field :field_id, :id
    field :constraint_id, :id

    timestamps()
  end

  @doc false
  def changeset(%ColumnConstraint{} = column, attrs) do
    column
    |> cast(attrs, [])
    |> validate_required([])
  end
end
