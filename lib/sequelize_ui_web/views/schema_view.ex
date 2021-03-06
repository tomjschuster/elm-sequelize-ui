defmodule SequelizeUiWeb.SchemaView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{SchemaView}

  def render("index.json", %{schemas: schemas}) do
    %{data: render_many(schemas, SchemaView, "schema.json")}
  end

  def render("show.json", %{schema: schema}) do
    %{data: render_one(schema, SchemaView, "schema.json")}
  end

  def render("schema.json", %{schema: schema}) do
    %{id: schema.id,
      name: schema.name}
  end
end
