module Data.Constraint exposing (Constraint)

import Data.Entity exposing (Entity)
import Data.Field exposing (Field)


type alias Constraint =
    { tipe : ConstraintType
    , name : String
    }


type ConstraintType
    = EntityConstraint Entity SqlConstraint
    | FieldConstraint Field SqlConstraint


type SqlConstraint
    = PrimaryKey
    | NotNull
    | Unique (List Int)
    | ForeignKey Int Field
    | Check String
