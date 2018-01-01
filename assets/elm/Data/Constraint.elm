module Data.Constraint
    exposing
        ( Constraint(..)
        , DefaultValue(..)
        , ForeignKey
        , NotNull
        , PrimaryKey
        , UniqueKey
        , columnId
        , columnInComposite
        , decoder
        , defaultValue
        , defaultValueDecoder
        , foreignKeyDecoder
        , hasColumn
        , inPrimaryKey
        , inSingleForeignKey
        , isNotNull
        , isSingleReference
        , isUnique
        , notNullDecoder
        , primaryKeyDecoder
        , primaryKeyIds
        , singleReference
        , uniqueKeyDecoder
        )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, required)
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



-- CONFIG


type alias Config =
    { constraintType : ConstraintType
    , name : String
    , decoder : Decoder Constraint
    }


type ConstraintType
    = PKType
    | NNType
    | DVType
    | UQType
    | FKType
    | NoConstraintType


toConfig : ConstraintType -> Config
toConfig constraintType =
    case constraintType of
        PKType ->
            { constraintType = PKType
            , name = "primary_key"
            , decoder = JD.map PK primaryKeyDecoder
            }

        NNType ->
            { constraintType = NNType
            , name = "not_null"
            , decoder = JD.map NN notNullDecoder
            }

        DVType ->
            { constraintType = DVType
            , name = "default_value"
            , decoder = JD.map DV defaultValueDecoder
            }

        UQType ->
            { constraintType = UQType
            , name = "unique_key"
            , decoder = JD.map UQ uniqueKeyDecoder
            }

        FKType ->
            { constraintType = FKType
            , name = "foreign_key"
            , decoder = JD.map FK foreignKeyDecoder
            }

        NoConstraintType ->
            { constraintType = NoConstraintType
            , name = ""
            , decoder = JD.fail "constraint fail"
            }


typeFromName : String -> ConstraintType
typeFromName name =
    case name of
        "primary_key" ->
            PKType

        "not_null" ->
            NNType

        "default_value" ->
            DVType

        "unique_key" ->
            UQType

        "foreign_key" ->
            FKType

        _ ->
            NoConstraintType



-- DECODERS


decoder : Decoder Constraint
decoder =
    JD.field "type" JD.string
        |> JD.andThen (typeFromName >> toConfig >> .decoder)


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


primaryKeyIds : PrimaryKey -> List ColumnId
primaryKeyIds (PrimaryKey _ _ (Index columnIds)) =
    columnIds


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


isUnique : ColumnId -> UniqueKey -> Bool
isUnique id (UniqueKey _ _ index) =
    isIndex id index


inSingleForeignKey : ColumnId -> ForeignKey -> Bool
inSingleForeignKey id (ForeignKey _ _ index) =
    isForeignKeyIndex id index


isSingleReference : ForeignKey -> Bool
isSingleReference =
    singleReference >> (/=) Nothing


singleReference : ForeignKey -> Maybe ColumnId
singleReference (ForeignKey _ _ index) =
    case index of
        ForeignKeyIndex [ ( _, singleId ) ] ->
            Just singleId

        _ ->
            Nothing


columnId : Constraint -> Maybe ColumnId
columnId constraint =
    case constraint of
        PK (PrimaryKey _ _ (Index [ id ])) ->
            Just id

        NN (NotNull _ _ id) ->
            Just id

        DV (DefaultValue _ _ id _) ->
            Just id

        UQ (UniqueKey _ _ (Index [ id ])) ->
            Just id

        FK (ForeignKey _ _ (ForeignKeyIndex [ ( id, _ ) ])) ->
            Just id

        _ ->
            Nothing


columnInComposite : ColumnId -> Constraint -> Bool
columnInComposite id constraint =
    case constraint of
        PK (PrimaryKey _ _ (Index ids)) ->
            List.member id ids

        UQ (UniqueKey _ _ (Index ids)) ->
            List.member id ids

        FK (ForeignKey _ _ (ForeignKeyIndex pairs)) ->
            List.any (\( id1, id2 ) -> id1 == id || id2 == id) pairs

        _ ->
            False


hasColumn : ColumnId -> Constraint -> Bool
hasColumn id constraint =
    columnId constraint == Just id || columnInComposite id constraint



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
