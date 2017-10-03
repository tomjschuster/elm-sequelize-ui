defmodule SequelizeUi.Repo.Migrations.UniqueNames do
  use Ecto.Migration

  def change do
    create unique_index(:schema, [:name])
    create unique_index(:entity, [:schema_id, :name])
    create unique_index(:field, [:entity_id, :name])
  end
end
