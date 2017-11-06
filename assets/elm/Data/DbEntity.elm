module Data.DbEntity exposing (DbEntity(..))

import Data.Column exposing (Column)
import Data.Constraints exposing (TableConstraints)
import Data.Schema exposing (Schema)
import Data.Table exposing (Table)


type DbEntity
    = DbSchema Schema
    | DbNewSchema Schema
    | DbUpdatedSchema Schema
    | DbSchemas (List Schema)
    | DbTable Table
    | DbNewTable Table
    | DbUpdatedTable Table
    | DbTables (List Table)
    | DbReferenceTables (List Table)
    | DbColumn Column
    | DbNewColumn Column
    | DbUpdatedColumn Column
    | DbColumns (List Column)
    | DbReferenceColumns (List Column)
    | DbTableConstraints TableConstraints
