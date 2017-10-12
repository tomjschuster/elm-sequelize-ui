module Data.Constraint exposing (Constraint)


type alias Constraint =
    { tipe : ConstraintType
    , entityId : Int
    , name : String
    }


type ConstraintType
    = PrimaryKey Int
    | NotNull Int
    | Unique (List Int)
    | ForeignKey Int Int
    | Check String
