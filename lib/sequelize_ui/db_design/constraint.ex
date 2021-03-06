defmodule SequelizeUi.DbDesign.Constraint do
  use Ecto.Schema
  import Ecto.Changeset
  alias Ecto.Changeset
  alias SequelizeUi.DbDesign.{Constraint, ConstraintType, Schema, Table, Column, ColumnConstraint}


  schema "sql_constraint" do
    field :name, :string
    field :constraint_type_id, :id
    field :schema_id, :id
    field :table_id, :id
    field :value, :string

    belongs_to :constraint_type, ConstraintType, define_field: false
    belongs_to :schema, Schema, define_field: false
    belongs_to :table, Table, define_field: false

    many_to_many :columns, Column, join_through: ColumnConstraint
    many_to_many :reference_columns, Column, join_through: ColumnConstraint,
      join_keys: [constraint_id: :id, references_id: :id]
    has_many :column_constraints, ColumnConstraint, foreign_key: :constraint_id

    timestamps()
  end

  @doc false
  def changeset(%Constraint{} = constraint, attrs) do
    constraint
    |> cast(attrs, [:name, :constraint_type_id, :schema_id, :table_id, :value])
    |> validate_required([:constraint_type_id, :schema_id, :table_id])
    |> assoc_constraint(:constraint_type)
    |> assoc_constraint(:schema)
    |> assoc_constraint(:table)
    |> unique_constraint(:name, name: :sql_constraint_schema_id_name_index)
    |> validate_default_value()
  end

  defp validate_default_value(%Changeset{} = changeset) do
    validate_change(changeset, :constraint_type_id, fn _, type_id ->

      case {type_id, Map.get(changeset.changes, :value)} do
        {3, nil} -> [{:value, "can't be blank"}]
        _ -> []
      end
    end)
  end
end
