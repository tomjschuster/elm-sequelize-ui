module Views.ChangesetError exposing (prependIfErrors, view)

import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Html exposing (Html, h3, h4, li, section, text, ul)
import Html.Attributes exposing (class)


prependIfErrors : List ChangesetError -> List (Html msg) -> List (Html msg)
prependIfErrors errors content =
    case errors of
        [] ->
            content

        x :: xs ->
            view errors :: content


view : List ChangesetError -> Html msg
view errors =
    section
        [ class "changeset-errors" ]
        [ heading, ul [] (List.map errorList errors |> List.concat) ]


heading : Html msg
heading =
    h3 [] [ text "Errors" ]


errorList : ChangesetError -> List (Html msg)
errorList { field, messages } =
    List.map (messageView field) messages


messageView : String -> String -> Html msg
messageView string message =
    li [] [ text (string ++ " " ++ message) ]
