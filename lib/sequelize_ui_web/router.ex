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
  # scope "/api", SequelizeUiWeb do
  #   pipe_through :api


  #   resources "/constraints", ConstraintController
  #   # get "/schmeas/:schema_id/tables", TableController, :for_schema
  #   pipe_through :combine_entities
  #   resources "/schemas", SchemaController do
  #     get "/tables", TableController, :for_schema
  #   end
  #   resources "/tables", TableController do
  #     get "/columns", ColumnController, :for_table
  #   end
  #   resources "/columns", ColumnController
  # end

  # scope "/api", SequelizeUiWeb do
  #   pipe_through :api

  #   resources "/schemas", SchemaController do
  #     resources "/tables", TableController do
  #       resources "/columns", ColumnController
  #       resources "/constraints", ConstraintController
  #     end
  #   end
  # end

  scope "/api", SequelizeUiWeb do
    pipe_through :api

    scope "/schemas" do
      get "/", SchemaController, :index
      get "/:id", SchemaController, :show
      post "/", SchemaController, :create
      put "/:id", SchemaController, :update
      delete "/:id", SchemaController, :delete

      get "/:schema_id/tables", TableController, :index_for_schema
    end

    scope "/tables" do
      get "/", TableController, :index
      get "/:id", TableController, :show
      post "/", TableController, :create
      put "/:id", TableController, :update
      delete "/:id", TableController, :delete

      get "/:table_id/columns", ColumnController, :index_for_table
      get "/:table_id/constraints", ConstraintController, :index_for_table
      get "/:table_id/references", ColumnController, :index_references
    end

    scope "/columns" do
      get "/", ColumnController, :index
      get "/:id", ColumnController, :show
      post "/", ColumnController, :create
      put "/:id", ColumnController, :update
      delete "/:id", ColumnController, :delete
    end

    scope "/constraints" do
      get "/", ConstraintController, :index
      get "/:id", ConstraintController, :show
      post "/", ConstraintController, :create
      put "/:id", ConstraintController, :update
      delete "/:id", ConstraintController, :delete
    end
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
