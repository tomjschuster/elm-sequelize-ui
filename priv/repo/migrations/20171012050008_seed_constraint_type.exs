defmodule SequelizeUi.Repo.Migrations.SeedConstraintType do
  use Ecto.Migration
  alias SequelizeUi.Repo
  alias SequelizeUi.DbDesign.ConstraintType


  def up do
    Repo.insert!(%ConstraintType{id: 1, name: "primary_key" })
    Repo.insert!(%ConstraintType{id: 2, name: "not_null" })
    Repo.insert!(%ConstraintType{id: 3, name: "default_value" })
    Repo.insert!(%ConstraintType{id: 4, name: "unique_key" })
    Repo.insert!(%ConstraintType{id: 5, name: "foreign_key" })
    Repo.insert!(%ConstraintType{id: 6, name:  "check" })
  end

  def down do
    Repo.delete_all(ConstraintType)
  end
end
