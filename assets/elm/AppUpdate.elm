module AppUpdate exposing (AppUpdate(..), generalError, hideError, httpError, none)

import Http


type AppUpdate
    = GeneralError String
    | HttpError Http.Error
    | HideError
    | None


generalError : String -> AppUpdate
generalError =
    GeneralError


httpError : Http.Error -> AppUpdate
httpError =
    HttpError


hideError : AppUpdate
hideError =
    HideError


none : AppUpdate
none =
    None
