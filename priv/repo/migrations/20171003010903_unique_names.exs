defmodule SequelizeUi.Repo.Migrations.UniqueNames do
  use Ecto.Migration

  def change do
    create unique_index(:schema, [:name])
    create unique_index(:sql_table, [:schema_id, :name])
    create unique_index(:sql_column, [:table_id, :name])
  end
end
