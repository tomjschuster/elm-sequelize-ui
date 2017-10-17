defmodule SequelizeUi.Repo.Migrations.SeedConstraintType do
  use Ecto.Migration
  alias SequelizeUi.Repo
  alias SequelizeUi.DbDesign.ConstraintType


  def up do
    _constraint_type_01 =
      Repo.insert!(%ConstraintType{name: "Primary Key", enum_name: "primary_key" })
    _constraint_type_02 =
      Repo.insert!(%ConstraintType{name: "Not Null", enum_name: "not_null" })
    _constraint_type_03 =
      Repo.insert!(%ConstraintType{name: "Default Value", enum_name: "default_value" })
    _constraint_type_04 =
      Repo.insert!(%ConstraintType{name: "Unique Key", enum_name: "unique_key" })
    _constraint_type_05 =
      Repo.insert!(%ConstraintType{name: "Foreign Key", enum_name: "foreign_key" })
    _constraint_type_06 =
      Repo.insert!(%ConstraintType{name: "Check", enum_name:  "check" })
  end

  def down do
    Repo.delete_all(ConstraintType)
  end
end
