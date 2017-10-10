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
    = Check String
    | NotNull
    | Unique (List String)
    | References ForeignKey


type alias ForeignKey =
    { sourceFieldName : String
    , targetField : Field
    }
