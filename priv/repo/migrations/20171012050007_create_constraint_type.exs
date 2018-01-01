defmodule SequelizeUi.Repo.Migrations.CreateConstraintType do
  use Ecto.Migration

  def change do
    create table(:constraint_type) do
      add :name, :string, null: false

      timestamps()
    end

    create unique_index(:constraint_type, [:name])
  end
end
