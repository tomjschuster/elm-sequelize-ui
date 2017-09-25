module Data exposing (Schema, emptySchema)


type alias Schema =
    { id : Int
    , name : String
    }


emptySchema : Schema
emptySchema =
    Schema 0 ""
