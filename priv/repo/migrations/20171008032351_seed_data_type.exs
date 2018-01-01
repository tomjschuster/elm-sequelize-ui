defmodule SequelizeUi.Repo.Migrations.SeedDataType do
  use Ecto.Migration
  alias SequelizeUi.Repo
  alias SequelizeUi.DbDesign.DataType


  def up do
    Repo.insert!(%DataType{id: 1, name: "char"})
    Repo.insert!(%DataType{id: 2, name: "varchar"})
    Repo.insert!(%DataType{id: 3, name: "text"})
    Repo.insert!(%DataType{id: 4, name: "bit"})
    Repo.insert!(%DataType{id: 5, name: "varbit"})
    Repo.insert!(%DataType{id: 6, name: "smallint"})
    Repo.insert!(%DataType{id: 7, name: "integer"})
    Repo.insert!(%DataType{id: 8, name: "bigint"})
    Repo.insert!(%DataType{id: 9, name: "smallserial"})
    Repo.insert!(%DataType{id: 10, name: "serial"})
    Repo.insert!(%DataType{id: 11, name: "bigserial"})
    Repo.insert!(%DataType{id: 12, name: "decimal"})
    Repo.insert!(%DataType{id: 13, name: "double"})
    Repo.insert!(%DataType{id: 14, name: "real"})
    Repo.insert!(%DataType{id: 15, name: "money"})
    Repo.insert!(%DataType{id: 16, name: "boolean"})
    Repo.insert!(%DataType{id: 17, name: "date"})
    Repo.insert!(%DataType{id: 18, name: "timestamp"})
    Repo.insert!(%DataType{id: 19, name: "time"})
  end

  def down do
    Repo.delete_all(DataType)
  end
end
