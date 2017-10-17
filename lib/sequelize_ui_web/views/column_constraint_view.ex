defmodule SequelizeUiWeb.ColumnConstraintView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.ColumnConstraintView

  def render("index.json", %{column_constraint: constraints}) do
    %{data: render_many(constraints, ColumnConstraintView, "column-constraint.json")}
  end

  def render("show.json", %{column_constraint: column_constraint}) do
    %{data: render_one(column_constraint, ColumnConstraintView, "column-constraint.json")}
  end

  def render("column-constraint.json", %{column_constraint: column_constraint}) do
    %{columnId: column_constraint.column_id,
      referencesId: column_constraint.references_id}
  end
end
