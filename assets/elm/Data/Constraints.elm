module Data.Constraints exposing (..)


type alias Constraints =
    { primaryKey : PrimaryKey
    , notNulls : List NotNull
    , defaultValues : List DefaultValue
    , uniqueKeys : List UniqueKey
    , foreignKeys : List ForeignKey
    }


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


type Index
    = Index Index
