defmodule SequelizeUiWeb.Router do
  use SequelizeUiWeb, :router
  import SequelizeUiWeb.CombinedWith

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :combine_entities do
    plug :combined_with
  end
  scope "/api", SequelizeUiWeb do
    pipe_through :api

    resources "/constraints", ConstraintController

    pipe_through :combine_entities
    resources "/schemas", SchemaController
    resources "/tables", TableController
    resources "/columns", ColumnController
  end

  scope "/", SequelizeUiWeb do
    pipe_through :browser # Use the default browser stack

    get "/*path", PageController, :index
  end

  # Other scopes may use custom stacks.
  # scope "/api", SequelizeUiWeb do
  #   pipe_through :api
  # end
end
