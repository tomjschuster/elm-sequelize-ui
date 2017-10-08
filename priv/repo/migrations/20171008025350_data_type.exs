defmodule SequelizeUi.Repo.Migrations.DataType do
  use Ecto.Migration

  def change do
    create table(:data_type) do
      add :string_value, :string, null: false
      timestamps()
    end

    create unique_index(:data_type, :string_value)

    alter table(:field) do
      add :data_type_id, references(:data_type)
      add :size, :integer
      add :precision, :integer
      add :decimals, :integer
      add :with_timezone, :boolean
    end
  end
end
