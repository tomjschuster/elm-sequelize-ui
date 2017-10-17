defmodule SequelizeUi.Repo.Migrations.CreateColumnConstraint do
  use Ecto.Migration

  def change do
    create table(:column_constraint) do
      add :column_id, references(:sql_column, on_delete: :delete_all), null: false
      add :constraint_id, references(:sql_constraint, on_delete: :delete_all), null: false
      add :references_id, references(:sql_column, on_delete: :delete_all)

      timestamps()
    end

    create index(:column_constraint, [:column_id])
    create index(:column_constraint, [:constraint_id])
    create index(:column_constraint, [:references_id])
  end
end
