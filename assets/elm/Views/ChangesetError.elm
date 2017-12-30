module Views.ChangesetError exposing (prependIfErrors, view)

import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Html exposing (Html, h3, li, section, text, ul)
import Html.Attributes as Attr


prependIfErrors : List ChangesetError -> List (Html msg) -> List (Html msg)
prependIfErrors errors content =
    case errors of
        [] ->
            content

        _ ->
            view errors :: content


view : List ChangesetError -> Html msg
view errors =
    section
        [ Attr.class "changeset-errors" ]
        [ heading, ul [] (List.map errorList errors |> List.concat) ]


heading : Html msg
heading =
    h3 [] [ text "Errors" ]


errorList : ChangesetError -> List (Html msg)
errorList { column, messages } =
    List.map (messageView column) messages


messageView : String -> String -> Html msg
messageView column message =
    li [] [ text (ChangesetError.columnToText column ++ " " ++ message) ]
