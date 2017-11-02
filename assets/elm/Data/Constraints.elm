module Data.Constraints
    exposing
        ( DefaultValue
        , ForeignKey
        , NotNull
        , PrimaryKey
        , TableConstraints
        , UniqueKey
        , decoder
        , defaultValue
        , defaultValueDecoder
        , emptyTableConstraints
        , foreignKeyDecoder
        , inPrimaryKey
        , inSingleForeignKey
        , isNotNull
        , isUnique
        , notNullDecoder
        , primaryKeyDecoder
        , singleReference
        , tableConstraintsDecoder
        , uniqueKeyDecoder
        )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)
import Utils.Serialization exposing (listSingletonDecoder)


-- TYPES


type Constraint
    = PK PrimaryKey
    | NN NotNull
    | DV DefaultValue
    | UQ UniqueKey
    | FK ForeignKey


type PrimaryKey
    = PrimaryKey ConstraintId ConstraintName Index


type NotNull
    = NotNull ConstraintId ConstraintName ColumnId


type DefaultValue
    = DefaultValue ConstraintId ConstraintName ColumnId String


type UniqueKey
    = UniqueKey ConstraintId ConstraintName Index


type ForeignKey
    = ForeignKey ConstraintId ConstraintName ForeignKeyIndex


type alias TableConstraints =
    { primaryKey : Maybe PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


emptyTableConstraints : TableConstraints
emptyTableConstraints =
    { primaryKey = Nothing
    , notNulls = []
    , defaultValues = []
    , uniqueKeys = []
    , foreignKeys = []
    }



-- DECODERS


decoder : Decoder Constraint
decoder =
    JD.field "constraintTypeId" JD.int |> JD.andThen constraintTypeDecoder


constraintTypeDecoder : Int -> Decoder Constraint
constraintTypeDecoder constraintTypeId =
    case constraintTypeId of
        1 ->
            primaryKeyDecoder |> JD.map PK

        2 ->
            notNullDecoder |> JD.map NN

        3 ->
            defaultValueDecoder |> JD.map DV

        4 ->
            uniqueKeyDecoder |> JD.map UQ

        5 ->
            foreignKeyDecoder |> JD.map FK

        _ ->
            JD.fail "invalid constraint type id"


tableConstraintsDecoder : Decoder TableConstraints
tableConstraintsDecoder =
    JD.list decoder |> JD.map toTableConstraints


primaryKeyDecoder : Decoder PrimaryKey
primaryKeyDecoder =
    decode PrimaryKey
        |> custom constraintIdDecoder
        |> custom constraintNameDecoder
        |> custom indexDecoder


notNullDecoder : Decoder NotNull
notNullDecoder =
    decode NotNull
        |> custom constraintIdDecoder
        |> custom constraintNameDecoder
        |> custom singleColumnDecoder


defaultValueDecoder : Decoder DefaultValue
defaultValueDecoder =
    decode DefaultValue
        |> custom constraintIdDecoder
        |> custom constraintNameDecoder
        |> custom singleColumnDecoder
        |> required "value" JD.string


uniqueKeyDecoder : Decoder UniqueKey
uniqueKeyDecoder =
    decode UniqueKey
        |> custom constraintIdDecoder
        |> custom constraintNameDecoder
        |> custom indexDecoder


foreignKeyDecoder : Decoder ForeignKey
foreignKeyDecoder =
    decode ForeignKey
        |> custom constraintIdDecoder
        |> custom constraintNameDecoder
        |> custom foreignKeyIndexDecoder


singleColumnDecoder : Decoder ColumnId
singleColumnDecoder =
    JD.field "columns" (JD.list columnIdDecoder) |> JD.andThen listSingletonDecoder


toTableConstraints : List Constraint -> TableConstraints
toTableConstraints constraints =
    List.foldr updateTableConstraints emptyTableConstraints constraints


updateTableConstraints : Constraint -> TableConstraints -> TableConstraints
updateTableConstraints constraint tableConstraints =
    case constraint of
        PK primaryKey ->
            { tableConstraints | primaryKey = Just primaryKey }

        NN notNull ->
            { tableConstraints | notNulls = notNull :: tableConstraints.notNulls }

        DV defaultValue ->
            { tableConstraints | defaultValues = defaultValue :: tableConstraints.defaultValues }

        UQ uniqueKey ->
            { tableConstraints | uniqueKeys = uniqueKey :: tableConstraints.uniqueKeys }

        FK foreignKey ->
            { tableConstraints | foreignKeys = foreignKey :: tableConstraints.foreignKeys }



-- Helper Types


type alias ConstraintId =
    Int


type alias ConstraintName =
    Maybe String


type alias ColumnId =
    Int


constraintIdDecoder : Decoder ConstraintId
constraintIdDecoder =
    JD.field "id" JD.int


constraintNameDecoder : Decoder ConstraintName
constraintNameDecoder =
    JD.field "name" (JD.maybe JD.string)


columnIdDecoder : Decoder ColumnId
columnIdDecoder =
    JD.field "columnId" JD.int



-- INDEXES


type Index
    = Index (List ColumnId)


type ForeignKeyIndex
    = ForeignKeyIndex (List ( ColumnId, ColumnId ))


indexDecoder : Decoder Index
indexDecoder =
    JD.field "columns" (JD.list (JD.field "columnId" JD.int) |> JD.map Index)


foreignKeyIndexDecoder : Decoder ForeignKeyIndex
foreignKeyIndexDecoder =
    JD.field "columns"
        (JD.list
            (JD.map2 (,)
                (JD.field "columnId" JD.int)
                (JD.field "referencesId" JD.int)
            )
            |> JD.map ForeignKeyIndex
        )



-- EXPOSED FUNCTIONS


inPrimaryKey : ColumnId -> PrimaryKey -> Bool
inPrimaryKey id (PrimaryKey _ _ index) =
    inIndex id index


isNotNull : ColumnId -> NotNull -> Bool
isNotNull id (NotNull _ _ columnId) =
    id == columnId


defaultValue : ColumnId -> DefaultValue -> Maybe String
defaultValue id (DefaultValue _ _ columnId value) =
    if columnId == id then
        Just value
    else
        Nothing


inUnique : ColumnId -> UniqueKey -> Bool
inUnique id (UniqueKey _ _ index) =
    inIndex id index


isUnique : ColumnId -> UniqueKey -> Bool
isUnique id (UniqueKey _ _ index) =
    isIndex id index


inSingleForeignKey : ColumnId -> ForeignKey -> Bool
inSingleForeignKey id (ForeignKey _ _ index) =
    isForeignKeyIndex id index


singleReference : ForeignKey -> Maybe ColumnId
singleReference (ForeignKey _ _ index) =
    case index of
        ForeignKeyIndex [ ( singleId, _ ) ] ->
            Just singleId

        _ ->
            Nothing



-- HELPERS


inIndex : ColumnId -> Index -> Bool
inIndex id (Index ids) =
    List.member id ids


isIndex : ColumnId -> Index -> Bool
isIndex id (Index ids) =
    ids == [ id ]


isForeignKeyIndex : ColumnId -> ForeignKeyIndex -> Bool
isForeignKeyIndex id (ForeignKeyIndex indexPairs) =
    case indexPairs of
        [ ( foreignKey, _ ) ] ->
            foreignKey == id

        _ ->
            False
