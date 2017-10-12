defmodule SequelizeUi.DbDesign.Field do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Field, Table, DataType, Constraint, FieldConstraint}


  schema "field" do
    field :table_id, :integer
    field :name, :string
    field :data_type_id, :integer
    field :size, :integer
    field :precision, :integer
    field :decimals, :integer
    field :with_timezone, :boolean

    belongs_to :table, Table, define_field: false
    belongs_to :data_type, DataType, define_field: false
    many_to_many :constraints, Constraint, join_through: FieldConstraint

    timestamps()
  end

  @doc false
  def changeset(%Field{} = field, attrs) do
    field
    |> cast(attrs, [:table_id, :name, :data_type_id, :size, :precision, :decimals, :with_timezone])
    |> validate_required([:table_id, :name, :data_type_id])
    |> assoc_constraint(:table)
    |> assoc_constraint(:data_type)
    |> unique_constraint(:name, name: :field_table_id_name_index)
  end
end
