defmodule SequelizeUi.Repo.Migrations.CreateField do
  use Ecto.Migration

  def change do
    create table(:field) do
      add :name, :string, null: false
      add :table_id, references(:sql_table, on_delete: :delete_all), null: false

      timestamps()
    end

    create index(:field, [:table_id])
  end
end
