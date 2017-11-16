module Views.Column.ReferenceFields exposing (view)

import Data.Column as Column exposing (Column)
import Data.Column.Reference as Reference exposing (Reference(..))
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


view :
    (List Reference -> msg)
    -> List Reference
    -> List Table
    -> List Column
    -> Html msg
view toMsg references tables columns =
    if List.isEmpty tables then
        div [] []
    else
        div []
            [ ul [] (createReferences toMsg tables columns references)
            , button
                [ onClick (references |> Reference.add |> toMsg), type_ "button" ]
                [ text "Add Association" ]
            ]


createReferences :
    (List Reference -> msg)
    -> List Table
    -> List Column
    -> List Reference
    -> List (Html msg)
createReferences toMsg tables columns references =
    List.indexedMap (createReference toMsg references tables columns) references


createReference :
    (List Reference -> msg)
    -> List Reference
    -> List Table
    -> List Column
    -> Int
    -> Reference
    -> Html msg
createReference toMsg references tables allColumns idx reference =
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

        Display tableId tableName columnId columnName ->
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
    (List Reference -> msg)
    -> List Reference
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
                [ onChangeInt (flip (Reference.selectTable idx) references >> toMsg) ]
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
    (List Reference -> msg)
    -> List Reference
    -> Int
    -> Maybe Int
    -> List Column
    -> Html msg
columnSelect toMsg references idx maybeColumnId columns =
    select
        [ onChangeInt (flip (Reference.selectColumn idx) references >> toMsg) ]
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


deleteButton : (List Reference -> msg) -> List Reference -> Int -> Html msg
deleteButton toMsg references idx =
    button
        [ onClick (Reference.delete idx references |> toMsg), type_ "button" ]
        [ text "Delete" ]
