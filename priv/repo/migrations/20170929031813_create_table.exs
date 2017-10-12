defmodule SequelizeUi.Repo.Migrations.CreateTable do
  use Ecto.Migration

  def change do
    create table(:sql_table) do
      add :name, :string, null: false
      add :schema_id, references(:schema, on_delete: :delete_all), null: false

      timestamps()
    end

    create index(:sql_table, [:schema_id])
  end
end
