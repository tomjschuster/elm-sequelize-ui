defmodule SequelizeUi.Repo.Migrations.DataType do
  use Ecto.Migration

  def change do
    create table(:data_type) do
      add :string_value, :string, null: false
      timestamps()
    end

    create unique_index(:data_type, :string_value)
  end
end
