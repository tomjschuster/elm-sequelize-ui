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

    scope "/schemas" do
      get "/", SchemaController, :index
      get "/:id", SchemaController, :show
      post "/", SchemaController, :create
      put "/:id", SchemaController, :update
      delete "/:id", SchemaController, :delete

      get "/:schema_id/tables", TableController, :index_for_schema
      get "/:schema_id/columns", ColumnController, :index_for_schema
      get "/:schema_id/candidates", TableController, :index_candidates
    end

    scope "/tables" do
      get "/", TableController, :index
      get "/:id", TableController, :show
      post "/", TableController, :create
      put "/:id", TableController, :update
      delete "/:id", TableController, :delete

      get "/:table_id/columns", ColumnController, :index_for_table
      get "/:table_id/constraints", ConstraintController, :index_for_table
      get "/:table_id/table-references", TableController, :index_references
      get "/:table_id/column-references", ColumnController, :index_references
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
end
