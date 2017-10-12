module Data.Constraint exposing (Constraint)


type alias Constraint =
    { tipe : ConstraintType
    , tableId : Int
    , name : String
    }


type ConstraintType
    = PrimaryKey Int
    | NotNull Int
    | Unique (List Int)
    | ForeignKey Int Int
    | Check String
