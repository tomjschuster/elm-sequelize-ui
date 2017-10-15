module Data.Constraints exposing (..)


type alias Constraints =
    { primaryKey : Maybe PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


type Index
    = Index (List Int)


type PrimaryKey
    = PrimaryKey String Index


type NotNull
    = NotNull String Int


type DefaultValue
    = DefaultValue String Int String


type UniqueKey
    = UniqueKey String Index


type ForeignKey
    = ForeignKey String Index Index


empty : Constraints
empty =
    { primaryKey = Nothing
    , notNulls = []
    , defaultValues = []
    , uniqueKeys = []
    , foreignKeys = []
    }


idInIndex : Int -> Index -> Bool
idInIndex id (Index ids) =
    List.member id ids


idIsIndex : Int -> Index -> Bool
idIsIndex id (Index ids) =
    ids == [ id ]


idInPrimaryKey : Int -> PrimaryKey -> Bool
idInPrimaryKey id (PrimaryKey _ index) =
    idInIndex id index


idIsNotNull : Int -> NotNull -> Bool
idIsNotNull id (NotNull _ notNullId) =
    id == notNullId


getDefaultValue : Int -> DefaultValue -> Maybe String
getDefaultValue id (DefaultValue _ defaultValueId defaultValue) =
    if defaultValueId == id then
        Just defaultValue
    else
        Nothing


idInUnique : Int -> UniqueKey -> Bool
idInUnique id (UniqueKey _ index) =
    idInIndex id index


idIsUnique : Int -> UniqueKey -> Bool
idIsUnique id (UniqueKey _ index) =
    idIsIndex id index


idInSingleForeignKey : Int -> ForeignKey -> Bool
idInSingleForeignKey id (ForeignKey _ sourceIndex _) =
    idIsIndex id sourceIndex


getSingleReference : ForeignKey -> Maybe Int
getSingleReference (ForeignKey _ _ (Index ids)) =
    case ids of
        [ singleId ] ->
            Just singleId

        _ ->
            Nothing



-- COLUMN CONSTRAINTS


type alias ColumnConstraints =
    { isPrimaryKey : Bool
    , isNotNull : Bool
    , defaultValue : Maybe String
    , isUnique : Bool
    , references : List Int
    }


defaultColumnConstraints : ColumnConstraints
defaultColumnConstraints =
    { isPrimaryKey = False
    , isNotNull = False
    , defaultValue = Nothing
    , isUnique = False
    , references = []
    }


getColumnConstraints : Int -> Constraints -> ColumnConstraints
getColumnConstraints columnId constraints =
    { isPrimaryKey = columnIsPrimaryKey columnId constraints.primaryKey
    , isNotNull = columnIsNotNull columnId constraints.notNulls
    , defaultValue = columnDefaultValue columnId constraints.defaultValues
    , isUnique = columnIsUnique columnId constraints.uniqueKeys
    , references = columnSingleReferences columnId constraints.foreignKeys
    }


columnIsPrimaryKey : Int -> Maybe PrimaryKey -> Bool
columnIsPrimaryKey columnId =
    Maybe.map (idInPrimaryKey columnId) >> Maybe.withDefault False


columnIsNotNull : Int -> List NotNull -> Bool
columnIsNotNull columnId =
    List.filter (idIsNotNull columnId) >> List.isEmpty >> not


columnDefaultValue : Int -> List DefaultValue -> Maybe String
columnDefaultValue columnId =
    List.filterMap (getDefaultValue columnId) >> List.head


columnIsUnique : Int -> List UniqueKey -> Bool
columnIsUnique columnId =
    List.filter (idIsUnique columnId) >> List.isEmpty >> not


columnSingleReferences : Int -> List ForeignKey -> List Int
columnSingleReferences columnId =
    List.filter (idInSingleForeignKey columnId) >> List.filterMap getSingleReference
