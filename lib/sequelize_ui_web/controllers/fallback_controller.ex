defmodule SequelizeUiWeb.FallbackController do
  @moduledoc """
  Translates controller action results into valid `Plug.Conn` responses.

  See `Phoenix.Controller.action_fallback/1` for more details.
  """
  use SequelizeUiWeb, :controller

  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(:unprocessable_entity)
    |> render(SequelizeUiWeb.ChangesetView, "error.json", changeset: changeset)
  end

  def call(conn, {:error, failed_op, %Ecto.Changeset{} = changeset, changes}) do
    conn
    |> put_status(:unprocessable_entity)
    |> render(SequelizeUiWeb.ChangesetView, "error.json", changeset: changeset)
  end

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> render(SequelizeUiWeb.ErrorView, :"404")
  end

  def call(conn, what_is_it?) do
    IO.inspect(what_is_it?)
    conn
    |> put_status(:internal_server_error)
    |> render(SequelizeUiWeb.ErrorView, :"500")
  end
end
