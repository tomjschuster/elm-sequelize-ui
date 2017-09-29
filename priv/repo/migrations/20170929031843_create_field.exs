defmodule SequelizeUi.Repo.Migrations.CreateField do
  use Ecto.Migration

  def change do
    create table(:field) do
      add :name, :string, null: false
      add :entity_id, references(:entity, on_delete: :nothing), null: false

      timestamps()
    end

    create index(:field, [:entity_id])
  end
end
