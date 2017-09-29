defmodule SequelizeUi.Repo.Migrations.CreateSchema do
  use Ecto.Migration

  def change do
    create table(:schema) do
      add :name, :string, null: false

      timestamps()
    end

  end
end
