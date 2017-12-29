module Data.DbEntity exposing (DbEntity(..))

import Data.Column exposing (Column)
import Data.Constraint exposing (Constraint)
import Data.Schema exposing (Schema)
import Data.Table exposing (Table)


type DbEntity
    = -- Schema
      DbSchema Schema
    | DbSchemas (List Schema)
    | DbNewSchema Schema
    | DbUpdatedSchema Schema
    | DbSchemaTables (List Table)
    | DbSchemaColumns (List Column)
      -- Table
    | DbTable Table
    | DbTables (List Table)
    | DbNewTable Table
    | DbUpdatedTable Table
      -- Column
    | DbColumn Column
    | DbColumns (List Column)
    | DbNewColumn Column
    | DbUpdatedColumn Column
      -- Constraint
    | DbConstraint Constraint
    | DbConstraints (List Constraint)
    | DbNewConstraint Constraint
    | DbUpdatedConstraint Constraint
