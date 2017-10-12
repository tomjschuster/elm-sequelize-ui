defmodule SequelizeUi.Repo.Migrations.CreateConstraint do
  use Ecto.Migration

  def change do
    create table(:sql_constraint_type) do
      add :name, :string, null: false
    end

    create table(:sql_constraint) do
      add :name, :string, null: false
      add :sql_constraint_type_id, references(:sql_constraint_type), null: false
      add :schema_id ,references(:schema), null: false
    end

    create table(:sql_constraint_field) do
      add :sql_constraint_id, references(:sql_constraint, on_delete: :delete_all)
      add :field_id, references(:field, on_delete: :delete_all)
    end
  end
end
