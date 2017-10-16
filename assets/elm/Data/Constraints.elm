module Data.Constraints
    exposing
        ( DefaultValue
        , ForeignKey
        , NotNull
        , PrimaryKey
        , UniqueKey
        , defaultValue
        , inPrimaryKey
        , inSingleForeignKey
        , isNotNull
        , isUnique
        , singleReference
        )

-- CONSTRAINTS


type PrimaryKey
    = PrimaryKey { id : Int, name : String, index : Index }


type NotNull
    = NotNull { id : Int, name : String, columnId : Int }


type DefaultValue
    = DefaultValue { id : Int, name : String, columnId : Int, value : String }


type UniqueKey
    = UniqueKey { id : Int, name : String, index : Index }


type ForeignKey
    = ForeignKey { id : Int, name : String, index : ForeignKeyIndex }



-- INDEXES


type Index
    = Index (List Int)


type ForeignKeyIndex
    = ForeignKeyIndex (List ( Int, Int ))



-- EXPOSED FUNCTIONS


inPrimaryKey : Int -> PrimaryKey -> Bool
inPrimaryKey id (PrimaryKey { index }) =
    inIndex id index


isNotNull : Int -> NotNull -> Bool
isNotNull id (NotNull { columnId }) =
    id == columnId


defaultValue : Int -> DefaultValue -> Maybe String
defaultValue id (DefaultValue { columnId, value }) =
    if columnId == id then
        Just value
    else
        Nothing


inUnique : Int -> UniqueKey -> Bool
inUnique id (UniqueKey { index }) =
    inIndex id index


isUnique : Int -> UniqueKey -> Bool
isUnique id (UniqueKey { index }) =
    isIndex id index


inSingleForeignKey : Int -> ForeignKey -> Bool
inSingleForeignKey id (ForeignKey { index }) =
    isForeignKeyIndex id index


singleReference : ForeignKey -> Maybe Int
singleReference (ForeignKey { index }) =
    case index of
        ForeignKeyIndex [ ( singleId, _ ) ] ->
            Just singleId

        _ ->
            Nothing



-- HELPERS


inIndex : Int -> Index -> Bool
inIndex id (Index ids) =
    List.member id ids


isIndex : Int -> Index -> Bool
isIndex id (Index ids) =
    ids == [ id ]


isForeignKeyIndex : Int -> ForeignKeyIndex -> Bool
isForeignKeyIndex id (ForeignKeyIndex indexPairs) =
    case indexPairs of
        [ ( foreignKey, _ ) ] ->
            foreignKey == id

        _ ->
            False
