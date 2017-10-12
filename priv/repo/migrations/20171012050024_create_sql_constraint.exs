defmodule SequelizeUi.Repo.Migrations.CreateSqlConstraint do
  use Ecto.Migration

  def change do
    create table(:sql_constraint) do
      add :name, :string, null: false
      add :constraint_type_id, references(:constraint_type, on_delete: :nothing), null: false
      add :table_id, references(:sql_table, on_delete: :nothing), null: false
      add :schema_id, references(:schema, on_delete: :nothing), null: false

      timestamps()
    end

    create unique_index(:sql_constraint, [:name, :schema_id])
    create index(:sql_constraint, [:constraint_type_id])
    create index(:sql_constraint, [:schema_id])
  end
end
