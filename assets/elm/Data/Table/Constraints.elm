module Data.Table.Constraints
    exposing
        ( TableConstraints
        , decoder
        , default
        , fromList
        )

import Data.Constraint as Constraint
    exposing
        ( Constraint
        , DefaultValue
        , ForeignKey
        , NotNull
        , PrimaryKey
        , UniqueKey
        )
import Json.Decode as JD exposing (Decoder)


type alias TableConstraints =
    { primaryKey : Maybe PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


default : TableConstraints
default =
    { primaryKey = Nothing
    , notNulls = []
    , defaultValues = []
    , uniqueKeys = []
    , foreignKeys = []
    }


fromList : List Constraint -> TableConstraints
fromList constraints =
    List.foldr update default constraints


update : Constraint -> TableConstraints -> TableConstraints
update constraint tableConstraints =
    case constraint of
        Constraint.PK primaryKey ->
            { tableConstraints | primaryKey = Just primaryKey }

        Constraint.NN notNull ->
            { tableConstraints | notNulls = notNull :: tableConstraints.notNulls }

        Constraint.DV defaultValue ->
            { tableConstraints | defaultValues = defaultValue :: tableConstraints.defaultValues }

        Constraint.UQ uniqueKey ->
            { tableConstraints | uniqueKeys = uniqueKey :: tableConstraints.uniqueKeys }

        Constraint.FK foreignKey ->
            { tableConstraints | foreignKeys = foreignKey :: tableConstraints.foreignKeys }



-- DECODE


decoder : Decoder TableConstraints
decoder =
    JD.list Constraint.decoder |> JD.map fromList
