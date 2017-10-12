defmodule SequelizeUi.Repo.Migrations.ModifyFieldWithDataType do
  use Ecto.Migration

  def change do
    alter table(:field) do
      add :data_type_id, references(:data_type), null: false
      add :size, :integer
      add :precision, :integer
      add :decimals, :integer
      add :with_timezone, :boolean
    end
  end
end
