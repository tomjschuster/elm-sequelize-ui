defmodule SequelizeUiWeb.ConstraintView do
  use SequelizeUiWeb, :view
  alias SequelizeUiWeb.{ConstraintView, ColumnConstraintView}

  def render("index.json", %{constraints: constraints}) do
    %{data: render_many(constraints, ConstraintView, "constraint.json")}
  end

  def render("show.json", %{constraint: constraint}) do
    %{data: render_one(constraint, ConstraintView, "constraint.json")}
  end

  def render("table-constraints.json", %{constraints: constraints}) do
    table_constraints =
      Enum.group_by(
        constraints,
        &(&1.constraint_type.enum_name),
        &render_one(&1, ConstraintView, "constraint.json")
      )
    %{primaryKey: table_constraints |> Map.get("primary_key") |> List.first,
      notNulls: table_constraints |> Map.get("not_null", []),
      defaultValues: table_constraints |> Map.get("default_value", []),
      uniqueKey: table_constraints |> Map.get("unique_key", []),
      foreignKeys: table_constraints |> Map.get("foreign_key", [])}
  end

  def render("constraint.json", %{constraint: constraint}) do
    %{id: constraint.id,
      name: constraint.name,
      constraintTypeId: constraint.constraint_type_id,
      value: constraint.value,
      columns:
        render_many(
          constraint.column_constraints,
          ColumnConstraintView,
          "column-constraint.json"
        )}
  end
end
