# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     SequelizeUi.Repo.insert!(%SequelizeUi.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias SequelizeUi.Repo
alias SequelizeUi.DbDesign.DataType

data_type_01 = Repo.insert!(%DataType{string_value: "char"})
data_type_02 = Repo.insert!(%DataType{string_value: "varchar"})
data_type_03 = Repo.insert!(%DataType{string_value: "text"})
data_type_04 = Repo.insert!(%DataType{string_value: "bit"})
data_type_05 = Repo.insert!(%DataType{string_value: "varbit"})
data_type_06 = Repo.insert!(%DataType{string_value: "smallint"})
data_type_07 = Repo.insert!(%DataType{string_value: "integer"})
data_type_08 = Repo.insert!(%DataType{string_value: "bigint"})
data_type_09 = Repo.insert!(%DataType{string_value: "smallserial"})
data_type_10 = Repo.insert!(%DataType{string_value: "serial"})
data_type_11 = Repo.insert!(%DataType{string_value: "bigserial"})
data_type_12 = Repo.insert!(%DataType{string_value: "numeric"})
data_type_13 = Repo.insert!(%DataType{string_value: "double"})
data_type_14 = Repo.insert!(%DataType{string_value: "real"})
data_type_15 = Repo.insert!(%DataType{string_value: "money"})
data_type_16 = Repo.insert!(%DataType{string_value: "boolean"})
data_type_17 = Repo.insert!(%DataType{string_value: "date"})
data_type_18 = Repo.insert!(%DataType{string_value: "timestamp"})
data_type_19 = Repo.insert!(%DataType{string_value: "time"})
