defmodule SequelizeUiWeb.ConstraintView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{ConstraintView, ColumnConstraintView}
  alias SequelizeUi.DbDesign.ConstraintType

  def render("index.json", %{constraints: constraints}) do
    %{data: render_many(constraints, ConstraintView, "constraint.json")}
  end

  def render("show.json", %{constraint: constraint}) do
    %{data: render_one(constraint, ConstraintView, "constraint.json")}
  end

  def render("constraint.json", %{constraint: constraint}) do
    %{id: constraint.id,
      name: constraint.name,
      type: constraint.constraint_type.name,
      value: constraint.value,
      columns:
        render_many(
          constraint.column_constraints,
          ColumnConstraintView,
          "column-constraint.json"
        )}
  end
end
