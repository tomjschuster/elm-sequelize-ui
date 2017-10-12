defmodule SequelizeUi.Repo.Migrations.CreateFieldConstraint do
  use Ecto.Migration

  def change do
    create table(:field_constraint) do
      add :field_id, references(:field, on_delete: :nothing), null: false
      add :constraint_id, references(:sql_constraint, on_delete: :nothing), null: false

      timestamps()
    end

    create index(:field_constraint, [:field_id])
    create index(:field_constraint, [:constraint_id])
  end
end
