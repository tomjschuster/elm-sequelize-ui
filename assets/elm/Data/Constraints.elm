module Data.Constraints
    exposing
        ( ColumnConstraints
        , Constraints
        , defaultColumnConstraints
        , empty
        , updateColumnDefaultValue
        , updateColumnHasDefaultValue
        , updateColumnIsNotNull
        , updateColumnIsPrimaryKey
        , updateColumnIsUnique
        )


type alias Constraints =
    { primaryKey : Maybe PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


type Index
    = Index (List Int)


type ForeignKeyIndex
    = ForeignKeyIndex (List ( Int, Int ))


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


idIsForeignKeyIndex : Int -> ForeignKeyIndex -> Bool
idIsForeignKeyIndex id (ForeignKeyIndex indexPairs) =
    case indexPairs of
        [ ( foreignKey, _ ) ] ->
            foreignKey == id

        _ ->
            False


idInPrimaryKey : Int -> PrimaryKey -> Bool
idInPrimaryKey id (PrimaryKey { index }) =
    idInIndex id index


idIsNotNull : Int -> NotNull -> Bool
idIsNotNull id (NotNull { columnId }) =
    id == columnId


getDefaultValue : Int -> DefaultValue -> Maybe String
getDefaultValue id (DefaultValue { columnId, value }) =
    if columnId == id then
        Just value
    else
        Nothing


idInUnique : Int -> UniqueKey -> Bool
idInUnique id (UniqueKey { index }) =
    idInIndex id index


idIsUnique : Int -> UniqueKey -> Bool
idIsUnique id (UniqueKey { index }) =
    idIsIndex id index


idInSingleForeignKey : Int -> ForeignKey -> Bool
idInSingleForeignKey id (ForeignKey { index }) =
    idIsForeignKeyIndex id index


getSingleReference : ForeignKey -> Maybe Int
getSingleReference (ForeignKey { index }) =
    case index of
        ForeignKeyIndex [ ( singleId, _ ) ] ->
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



-- CHECK VALUES


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



-- UPDATE VALUES


updateColumnIsPrimaryKey : Bool -> ColumnConstraints -> ColumnConstraints
updateColumnIsPrimaryKey isPrimaryKey constraints =
    { constraints | isPrimaryKey = isPrimaryKey }


updateColumnIsNotNull : Bool -> ColumnConstraints -> ColumnConstraints
updateColumnIsNotNull isNotNull constraints =
    { constraints | isNotNull = isNotNull }


updateColumnHasDefaultValue : Bool -> ColumnConstraints -> ColumnConstraints
updateColumnHasDefaultValue hasDefaultValue constraints =
    if hasDefaultValue then
        { constraints | defaultValue = Just "" }
    else
        { constraints | defaultValue = Nothing }


updateColumnDefaultValue : String -> ColumnConstraints -> ColumnConstraints
updateColumnDefaultValue defaultValue constraints =
    { constraints | defaultValue = Just defaultValue }


updateColumnIsUnique : Bool -> ColumnConstraints -> ColumnConstraints
updateColumnIsUnique isUnique constraints =
    { constraints | isUnique = isUnique }
