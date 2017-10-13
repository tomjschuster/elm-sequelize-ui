defmodule SequelizeUi.Repo.Migrations.CreateColumnConstraint do
  use Ecto.Migration

  def change do
    create table(:column_constraint) do
      add :column_id, references(:sql_column, on_delete: :nothing), null: false
      add :constraint_id, references(:sql_constraint, on_delete: :nothing), null: false

      timestamps()
    end

    create index(:column_constraint, [:column_id])
    create index(:column_constraint, [:constraint_id])
  end
end
