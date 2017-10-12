defmodule SequelizeUi.Repo.Migrations.SeedConstraintType do
  use Ecto.Migration
  alias SequelizeUi.Repo
  alias SequelizeUi.DbDesign.ConstraintType


  def up do
    _constraint_type_01 = Repo.insert!(%ConstraintType{name: "Primary Key"})
    _constraint_type_02 = Repo.insert!(%ConstraintType{name: "Not Null"})
    _constraint_type_03 = Repo.insert!(%ConstraintType{name: "Unique Key"})
    _constraint_type_04 = Repo.insert!(%ConstraintType{name: "Foreign Key"})
    _constraint_type_05 = Repo.insert!(%ConstraintType{name: "Check"})
  end

  def down do
    Repo.delete_all(ConstraintType)
  end
end
