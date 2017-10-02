defmodule SequelizeUi.Repo.Migrations.CreateEntity do
  use Ecto.Migration

  def change do
    create table(:entity) do
      add :name, :string, null: false
      add :schema_id, references(:schema, on_delete: :delete_all), null: false

      timestamps()
    end

    create index(:entity, [:schema_id])
  end
end
