module Data.Constraint
    exposing
        ( Constraint(..)
        , all
        , createCheck
        , createForeignKey
        , createNotNull
        , createPrimaryKey
        , createUniqueKey
        , fromId
        , none
        , toId
        , toName
        )

import Json.Encode as JE exposing (Value)


type Constraint
    = NoConstraint
    | PrimaryKey Int
    | NotNull Int
    | UniqueKey (List Int)
    | ForeignKey Int Int
    | Check (Maybe Int) String


type alias ConstraintConfig =
    { constraint : Constraint
    , id : Int
    , stringValue : String
    }


all : List Constraint
all =
    [ emptyPrimaryKey
    , emptyNotNull
    , emptyUniqueKey
    , emptyForeignKey
    , emptyCheck
    ]



-- DEFAULTS


none : Constraint
none =
    NoConstraint


emptyPrimaryKey : Constraint
emptyPrimaryKey =
    PrimaryKey 0


emptyNotNull : Constraint
emptyNotNull =
    NotNull 0


emptyUniqueKey : Constraint
emptyUniqueKey =
    UniqueKey []


emptyForeignKey : Constraint
emptyForeignKey =
    ForeignKey 0 0


emptyCheck : Constraint
emptyCheck =
    Check Nothing ""



-- UPDATE


createPrimaryKey : Int -> Constraint
createPrimaryKey =
    PrimaryKey


createNotNull : Int -> Constraint
createNotNull =
    NotNull


createUniqueKey : List Int -> Constraint
createUniqueKey =
    UniqueKey


createForeignKey : Int -> Int -> Constraint
createForeignKey =
    ForeignKey


createCheck : Maybe Int -> String -> Constraint
createCheck =
    Check



-- CONFIG


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

        UniqueKey columnIds ->
            { constraint = UniqueKey columnIds
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


toId : Constraint -> Int
toId =
    toConfig >> .id


fromId : Int -> Maybe Constraint
fromId id =
    case id of
        0 ->
            Just none

        1 ->
            Just emptyPrimaryKey

        2 ->
            Just emptyNotNull

        3 ->
            Just emptyUniqueKey

        4 ->
            Just emptyForeignKey

        5 ->
            Just emptyCheck

        badId ->
            Nothing
