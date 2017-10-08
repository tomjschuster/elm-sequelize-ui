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

Repo.insert!(%DataType{string_value: "char"})
Repo.insert!(%DataType{string_value: "varchar"})
Repo.insert!(%DataType{string_value: "text"})
Repo.insert!(%DataType{string_value: "bit"})
Repo.insert!(%DataType{string_value: "varbit"})
Repo.insert!(%DataType{string_value: "smallint"})
Repo.insert!(%DataType{string_value: "integer"})
Repo.insert!(%DataType{string_value: "bigint"})
Repo.insert!(%DataType{string_value: "smallserial"})
Repo.insert!(%DataType{string_value: "serial"})
Repo.insert!(%DataType{string_value: "bigserial"})
Repo.insert!(%DataType{string_value: "numeric"})
Repo.insert!(%DataType{string_value: "double"})
Repo.insert!(%DataType{string_value: "real"})
Repo.insert!(%DataType{string_value: "money"})
Repo.insert!(%DataType{string_value: "boolean"})
Repo.insert!(%DataType{string_value: "date"})
Repo.insert!(%DataType{string_value: "timestamp"})
Repo.insert!(%DataType{string_value: "time"})
