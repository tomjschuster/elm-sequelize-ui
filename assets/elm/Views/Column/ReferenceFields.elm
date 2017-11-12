module Views.Column.ReferenceFields exposing (Config, view)

import Array exposing (Array)
import Data.Column as Column exposing (Column, EditingReference(..))
import Data.Table as Table exposing (Table)
import Html
    exposing
        ( Html
        , button
        , div
        , li
        , option
        , p
        , select
        , text
        , ul
        )
import Html.Attributes as Attributes exposing (disabled, selected, type_, value)
import Html.Events as Events exposing (onClick)
import Utils.Handlers as Handlers exposing (onChangeInt)


type alias Config =
    {}


view :
    msg
    -> (Int -> Maybe Int -> msg)
    -> (Int -> Int -> List Column -> Maybe Int -> msg)
    -> (Int -> msg)
    -> Array EditingReference
    -> List Table
    -> Html msg
view addEditingReference toSelectEditingReferenceTable toSelectEditingReferenceColumn toRemoveEditingReference assocs tables =
    if List.isEmpty tables then
        div [] [ p [] [ text "No columns with the current data type exist in schema." ] ]
    else
        div []
            [ ul [] (createColumnAssocListItems toSelectEditingReferenceTable toSelectEditingReferenceColumn toRemoveEditingReference tables assocs)
            , button [ onClick addEditingReference, type_ "button" ] [ text "Add Association" ]
            ]


createColumnAssocListItems :
    (Int -> Maybe Int -> msg)
    -> (Int -> Int -> List Column -> Maybe Int -> msg)
    -> (Int -> msg)
    -> List Table
    -> Array EditingReference
    -> List (Html msg)
createColumnAssocListItems toSelectEditingReferenceTable toSelectEditingReferenceColumn toRemoveEditingReference tables =
    Array.toIndexedList >> List.map (uncurry (createColumnAssoc toSelectEditingReferenceTable toSelectEditingReferenceColumn toRemoveEditingReference tables))


createColumnAssoc :
    (Int -> Maybe Int -> msg)
    -> (Int -> Int -> List Column -> Maybe Int -> msg)
    -> (Int -> msg)
    -> List Table
    -> Int
    -> EditingReference
    -> Html msg
createColumnAssoc toSelectEditingReferenceTable toSelectEditingReferenceColumn toRemoveEditingReference tables idx assoc =
    case assoc of
        SelectTable ->
            li []
                [ tableSelect (toSelectEditingReferenceTable idx) Nothing tables
                , deleteNewAssocButton toRemoveEditingReference idx
                ]

        SelectColumn tableId columns ->
            li []
                [ tableSelect (toSelectEditingReferenceTable idx) (Just tableId) tables
                , columnSelect (toSelectEditingReferenceColumn idx tableId columns) Nothing columns
                , deleteNewAssocButton toRemoveEditingReference idx
                ]

        Ready tableId columns columnId ->
            li []
                [ tableSelect (toSelectEditingReferenceTable idx) (Just tableId) tables
                , columnSelect (toSelectEditingReferenceColumn idx tableId columns) (Just columnId) columns
                , deleteNewAssocButton toRemoveEditingReference idx
                ]


tableSelect : (Maybe Int -> msg) -> Maybe Int -> List Table -> Html msg
tableSelect toSelectTable maybeTableId tables =
    case tables of
        [] ->
            select [ disabled True ] [ option [] [ text "No for datatype" ] ]

        _ ->
            select
                [ onChangeInt toSelectTable ]
                (option [ selected (maybeTableId == Nothing) ]
                    [ text "Select a Table" ]
                    :: List.map (tableOption maybeTableId) tables
                )


tableOption : Maybe Int -> Table -> Html msg
tableOption maybeId { id, name } =
    option
        [ value (toString id)
        , selected (maybeId |> Maybe.map ((==) id) |> Maybe.withDefault False)
        ]
        [ text name ]


columnSelect : (Maybe Int -> msg) -> Maybe Int -> List Column -> Html msg
columnSelect toSelectColumn maybeColumnId columns =
    select
        [ onChangeInt toSelectColumn ]
        (option [ selected (maybeColumnId == Nothing) ] [ text "Select a Column" ]
            :: List.map (columnOption maybeColumnId) columns
        )


columnOption : Maybe Int -> Column -> Html msg
columnOption maybeId { id, name } =
    option
        [ value (toString id)
        , selected (maybeId |> Maybe.map ((==) id) |> Maybe.withDefault False)
        ]
        [ text name ]


deleteNewAssocButton : (Int -> msg) -> Int -> Html msg
deleteNewAssocButton toRemoveEditingReference idx =
    button
        [ onClick (toRemoveEditingReference idx), type_ "button" ]
        [ text "Delete" ]
