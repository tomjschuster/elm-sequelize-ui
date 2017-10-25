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
    # get "/schmeas/:schema_id/tables", TableController, :for_schema
    pipe_through :combine_entities
    resources "/schemas", SchemaController do
      get "/tables", TableController, :for_schema
    end
    resources "/tables", TableController do
      get "/columns", ColumnController, :for_table
    end
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
