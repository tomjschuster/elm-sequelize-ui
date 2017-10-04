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
        (heading :: List.map errorView errors)


heading : Html msg
heading =
    h3 [] [ text "Errors" ]


errorView : ChangesetError -> Html msg
errorView { field, messages } =
    section
        [ class "changeset-field-error" ]
        [ h4 [] [ text field ]
        , ul [] (List.map messageView messages)
        ]


messageView : String -> Html msg
messageView message =
    li [] [ text message ]
