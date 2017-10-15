module Data.Constraints exposing (..)


type alias Constraints =
    { primaryKey : Maybe PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


empty : Constraints
empty =
    { primaryKey = Nothing
    , notNulls = []
    , defaultValues = []
    , uniqueKeys = []
    , foreignKeys = []
    }


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
    { isPrimaryKey =
        Maybe.map
            (idInPrimaryKey columnId)
            constraints.primaryKey
            |> Maybe.withDefault False
    , isNotNull =
        List.filter
            (idIsNotNull columnId)
            constraints.notNulls
            |> List.isEmpty
            |> not
    , defaultValue =
        List.filterMap
            (getDefaultValue columnId)
            constraints.defaultValues
            |> List.head
    , isUnique =
        List.filter
            (idInUnique columnId)
            constraints.uniqueKeys
            |> List.isEmpty
            |> not
    , references = []
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
    = ForeignKey String ( Index, Index )


idInIndex : Int -> Index -> Bool
idInIndex id (Index ids) =
    List.member id ids


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
