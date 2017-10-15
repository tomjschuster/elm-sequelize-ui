defmodule SequelizeUi.Repo.Migrations.ModifyFieldWithDataType do
  use Ecto.Migration

  def change do
    alter table(:sql_column) do
      add :data_type_id, references(:data_type), null: false
      add :size, :integer
      add :precision, :integer
      add :scale, :integer
      add :with_timezone, :boolean
    end
  end
end
