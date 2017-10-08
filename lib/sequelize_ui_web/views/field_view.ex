defmodule SequelizeUiWeb.FieldView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{FieldView, EntityView, SchemaView}

  def render("index.json", %{fields: fields}) do
    %{data: render_many(fields, FieldView, "field.json")}
  end

  def render("show.json", %{field: field}) do
    %{data: render_one(field, FieldView, "field.json")}
  end

  def render("show-with-entity.json", %{field: field}) do
    %{data: %{field: render_one(field, FieldView, "field.json"),
              entity: render_one(field.entity, EntityView, "entity.json")}}
  end

  def render("show-with-all.json", %{field: field}) do
    %{data: %{field: render_one(field, FieldView, "field.json"),
              entity: render_one(field.entity, EntityView, "entity.json"),
              schema: render_one(field.entity.schema, SchemaView, "schema.json")}}
  end

  def render("field.json", %{field: field}) do
    %{id: field.id,
      name: field.name,
      entityId: field.entity_id,
      dataTypeId: field.data_type_id}
  end
end
