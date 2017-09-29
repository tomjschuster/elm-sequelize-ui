defmodule SequelizeUiWeb.PageController do
  use SequelizeUiWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
