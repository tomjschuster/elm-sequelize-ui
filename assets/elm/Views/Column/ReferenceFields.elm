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


type alias Config msg =
    { addEditingReference : msg
    , toSelectEditingReferenceTable : Int -> Maybe Int -> msg
    , toSelectEditingReferenceColumn : Int -> Int -> List Column -> Maybe Int -> msg
    , toRemoveEditingReference : Int -> msg
    }


view :
    (List EditingReference -> msg)
    -> List EditingReference
    -> List Table
    -> List Column
    -> Html msg
view toMsg references tables columns =
    if List.isEmpty tables then
        div [] [ p [] [ text "No columns with the current data type exist in schema." ] ]
    else
        div []
            [ ul [] (referenceFields toMsg tables columns references)
            , button [ onClick (references ++ [ Column.SelectTable ] |> toMsg), type_ "button" ] [ text "Add Association" ]
            ]


referenceFields :
    (List EditingReference -> msg)
    -> List Table
    -> List Column
    -> List EditingReference
    -> List (Html msg)
referenceFields toMsg tables columns references =
    List.indexedMap (referenceField toMsg references tables columns) references


referenceField :
    (List EditingReference -> msg)
    -> List EditingReference
    -> List Table
    -> List Column
    -> Int
    -> EditingReference
    -> Html msg
referenceField toMsg references tables allColumns idx reference =
    case reference of
        SelectTable ->
            li []
                [ tableSelect toMsg references idx Nothing tables
                , deleteButton toMsg references idx
                ]

        SelectColumn tableId ->
            let
                columns =
                    List.filter (.tableId >> (==) tableId) allColumns
            in
            li []
                [ tableSelect toMsg references idx (Just tableId) tables
                , columnSelect toMsg references idx Nothing columns
                , deleteButton toMsg references idx
                ]

        Ready tableId columnId ->
            let
                columns =
                    List.filter (.tableId >> (==) tableId) allColumns
            in
            li []
                [ tableSelect toMsg references idx (Just tableId) tables
                , columnSelect toMsg references idx (Just columnId) columns
                , deleteButton toMsg references idx
                ]


tableSelect :
    (List EditingReference -> msg)
    -> List EditingReference
    -> Int
    -> Maybe Int
    -> List Table
    -> Html msg
tableSelect toMsg references idx maybeTableId tables =
    case tables of
        [] ->
            select [ disabled True ] [ option [] [ text "No for datatype" ] ]

        _ ->
            select
                [ onChangeInt (flip (Column.selectTable idx) references >> toMsg) ]
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


columnSelect :
    (List EditingReference -> msg)
    -> List EditingReference
    -> Int
    -> Maybe Int
    -> List Column
    -> Html msg
columnSelect toMsg references idx maybeColumnId columns =
    select
        [ onChangeInt (flip (Column.selectColumn idx) references >> toMsg) ]
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


deleteButton : (List EditingReference -> msg) -> List EditingReference -> Int -> Html msg
deleteButton toMsg references idx =
    button
        [ onClick (Column.deleteReference idx references |> toMsg), type_ "button" ]
        [ text "Delete" ]
