defmodule SequelizeUi.DbDesign.Table do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Table, Schema, Column, Constraint}


  schema "sql_table" do
    field :name, :string
    field :schema_id, :integer

    belongs_to :schema, Schema, define_field: false
    has_many :columns, Column
    has_many :constraints, Constraint

    timestamps()
  end

  @doc false
  def changeset(%Table{} = table, attrs) do
    table
    |> cast(attrs, [:name, :schema_id])
    |> validate_required([:name, :schema_id])
    |> assoc_constraint(:schema)
    |> unique_constraint(:name, name: :table_schema_id_name_index)
  end
end
