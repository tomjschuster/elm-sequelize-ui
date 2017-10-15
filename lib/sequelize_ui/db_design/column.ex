defmodule SequelizeUi.DbDesign.Column do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.{Column, Table, DataType, Constraint, ColumnConstraint}


  schema "sql_column" do
    field :table_id, :integer
    field :name, :string
    field :data_type_id, :integer
    field :size, :integer
    field :precision, :integer
    field :scale, :integer
    field :with_timezone, :boolean

    belongs_to :table, Table, define_field: false
    belongs_to :data_type, DataType, define_field: false
    many_to_many :constraints, Constraint, join_through: ColumnConstraint

    timestamps()
  end

  @doc false
  def changeset(%Column{} = column, attrs) do
    column
    |> cast(attrs, [:table_id, :name, :data_type_id, :size, :precision, :scale, :with_timezone])
    |> validate_required([:table_id, :name, :data_type_id])
    |> assoc_constraint(:table)
    |> assoc_constraint(:data_type)
    |> unique_constraint(:name, name: :sql_column_table_id_name_index)
  end
end
