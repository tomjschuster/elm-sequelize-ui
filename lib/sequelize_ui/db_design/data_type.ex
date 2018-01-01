defmodule SequelizeUi.DbDesign.DataType do
  use Ecto.Schema
  import Ecto.Changeset
  alias SequelizeUi.DbDesign.DataType


  schema "data_type" do
    field :name, :string

    timestamps()
  end

  @doc false
  def changeset(%DataType{} = data_type, attrs) do
    data_type
    |> cast(attrs, [:name])
    |> validate_required([:name])
  end

  def id_to_atom(id) do
    case id do
      1 -> :char
      2 -> :varchar
      3 -> :text
      4 -> :bit
      5 -> :varbit
      6 -> :smallint
      7 -> :integer
      8 -> :bigint
      9 -> :smallserial
      10 -> :serial
      11 -> :bigserial
      12 -> :decimal
      13 -> :double
      14 -> :real
      15 -> :money
      16 -> :boolean
      17 -> :date
      18 -> :timestamp
      19 -> :time
    end
  end

  def atom_to_id(atom) do
    case atom do
      :char -> 1
      :varchar -> 2
      :text -> 3
      :bit -> 4
      :varbit -> 5
      :smallint -> 6
      :integer -> 7
      :bigint -> 8
      :smallserial -> 9
      :serial -> 10
      :bigserial -> 11
      :decimal -> 12
      :double -> 13
      :real -> 14
      :money -> 15
      :boolean -> 16
      :date -> 17
      :timestamp -> 18
      :time -> 19
    end
  end

  def string_to_id(string) do
    case string do
      "char" -> 1
      "varchar" -> 2
      "text" -> 3
      "bit" -> 4
      "varbit" -> 5
      "smallint" -> 6
      "integer" -> 7
      "bigint" -> 8
      "smallserial" -> 9
      "serial" -> 10
      "bigserial" -> 11
      "decimal" -> 12
      "double" -> 13
      "real" -> 14
      "money" -> 15
      "boolean" -> 16
      "date" -> 17
      "timestamp" -> 18
      "time" -> 19
    end
  end
end
