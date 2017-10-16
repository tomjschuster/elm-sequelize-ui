defmodule SequelizeUi.Repo.Migrations.SeedDataType do
  use Ecto.Migration
  alias SequelizeUi.Repo
  alias SequelizeUi.DbDesign.DataType


  def up do
    _data_type_01 = Repo.insert!(%DataType{string_value: "char"})
    _data_type_02 = Repo.insert!(%DataType{string_value: "varchar"})
    _data_type_03 = Repo.insert!(%DataType{string_value: "text"})
    _data_type_04 = Repo.insert!(%DataType{string_value: "bit"})
    _data_type_05 = Repo.insert!(%DataType{string_value: "varbit"})
    _data_type_06 = Repo.insert!(%DataType{string_value: "smallint"})
    _data_type_07 = Repo.insert!(%DataType{string_value: "integer"})
    _data_type_08 = Repo.insert!(%DataType{string_value: "bigint"})
    _data_type_09 = Repo.insert!(%DataType{string_value: "smallserial"})
    _data_type_10 = Repo.insert!(%DataType{string_value: "serial"})
    _data_type_11 = Repo.insert!(%DataType{string_value: "bigserial"})
    _data_type_12 = Repo.insert!(%DataType{string_value: "decimal"})
    _data_type_13 = Repo.insert!(%DataType{string_value: "double"})
    _data_type_14 = Repo.insert!(%DataType{string_value: "real"})
    _data_type_15 = Repo.insert!(%DataType{string_value: "money"})
    _data_type_16= Repo.insert!(%DataType{string_value: "boolean"})
    _data_type_17 = Repo.insert!(%DataType{string_value: "date"})
    _data_type_18 = Repo.insert!(%DataType{string_value: "timestamp"})
    _data_type_19 = Repo.insert!(%DataType{string_value: "time"})
  end

  def down do
    Repo.delete_all(DataType)
  end
end
