defmodule SequelizeUiWeb.CombinedWith do
  import Plug.Conn

  @entity_map %{schema: false, table: false, tables: false, columns: false}

  def combined_with(conn, _) do
    combined_with = split_combined_with(conn.params["with"])
    conn |> Plug.Conn.assign(:combined_with, combined_with)
  end

  defp split_combined_with(nil), do: @entity_map
  defp split_combined_with(with_string) do
    entities =
      with_string
      |> String.split(",")
      |> Enum.map(&safe_to_existing_atom/1)
      |> Enum.filter(&(&1))
      |> Enum.into(%{}, fn x -> {x, true} end)
      |> Map.merge(@entity_map, fn k, v1, v2 -> v1 end)
  end


  def safe_to_existing_atom(string) do
    try do
      String.to_existing_atom(string)
    rescue
      ArgumentError -> nil
    end
  end
end
