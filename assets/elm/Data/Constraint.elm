module Data.Constraint exposing (Constraint)


type Constraint
    = NoConstraint
    | PrimaryKey Int
    | NotNull Int
    | Unique (List Int)
    | ForeignKey Int Int
    | Check (Maybe Int) String


type alias ConstraintConfig =
    { constraint : Constraint
    , id : Int
    , stringValue : String
    }


toConfig : Constraint -> ConstraintConfig
toConfig constraint =
    case constraint of
        NoConstraint ->
            { constraint = NoConstraint
            , id = 0
            , stringValue = ""
            }

        PrimaryKey columnId ->
            { constraint = PrimaryKey columnId
            , id = 1
            , stringValue = "Primary Key"
            }

        NotNull columnId ->
            { constraint = NotNull columnId
            , id = 2
            , stringValue = "Not Null"
            }

        Unique columnIds ->
            { constraint = Unique columnIds
            , id = 3
            , stringValue = "Unique Key"
            }

        ForeignKey foreignKeyId referencesId ->
            { constraint = ForeignKey foreignKeyId referencesId
            , id = 4
            , stringValue = "Foreign Key"
            }

        Check maybeColumnId query ->
            { constraint = Check maybeColumnId query
            , id = 5
            , stringValue = "Check"
            }


toName : Constraint -> String
toName =
    toConfig >> .stringValue
