defmodule SequelizeUi.Repo.Migrations.CreateColumn do
  use Ecto.Migration

  def change do
    create table(:sql_column) do
      add :name, :string, null: false
      add :table_id, references(:sql_table, on_delete: :delete_all), null: false

      timestamps()
    end

    create index(:sql_column, [:table_id])
  end
end
