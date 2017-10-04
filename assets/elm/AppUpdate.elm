module AppUpdate exposing (AppUpdate(..), displayError, hideError, none)


type AppUpdate
    = DisplayError String
    | HideError
    | None


displayError : String -> AppUpdate
displayError =
    DisplayError


hideError : AppUpdate
hideError =
    HideError


none : AppUpdate
none =
    None
