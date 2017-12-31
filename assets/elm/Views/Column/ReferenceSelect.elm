module Views.Column.ReferenceSelect exposing (view)

import Data.Column exposing (Column)
import Data.Column.Reference as Reference exposing (Reference(..))
import Data.Table exposing (Table)
import Html
    exposing
        ( Html
        , button
        , option
        , select
        , span
        , text
        )
import Html.Attributes as Attr
import Html.Events as Evt
import Utils.Events as EvtUtils


view :
    (Reference -> msg)
    -> List Table
    -> List Column
    -> Reference
    -> Html msg
view toMsg tables allColumns reference =
    case reference of
        None ->
            span []
                [ tableSelect toMsg reference Nothing tables
                ]

        TableSelected tableId ->
            let
                columns =
                    List.filter (.tableId >> (==) tableId) allColumns
            in
            span []
                [ tableSelect toMsg reference (Just tableId) tables
                , columnSelect toMsg reference Nothing columns
                , clearButton toMsg
                ]

        Complete tableId columnId ->
            let
                columns =
                    List.filter (.tableId >> (==) tableId) allColumns
            in
            span []
                [ tableSelect toMsg reference (Just tableId) tables
                , columnSelect toMsg reference (Just columnId) columns
                , clearButton toMsg
                ]


tableSelect :
    (Reference -> msg)
    -> Reference
    -> Maybe Int
    -> List Table
    -> Html msg
tableSelect toMsg reference maybeTableId tables =
    case tables of
        [] ->
            select [ Attr.disabled True ] [ option [] [ text "No available columns" ] ]

        _ ->
            select
                [ EvtUtils.onChangeInt (flip Reference.selectTable reference >> toMsg) ]
                (option [ Attr.selected (maybeTableId == Nothing) ]
                    [ text "Select a Table" ]
                    :: List.map (tableOption maybeTableId) tables
                )


tableOption : Maybe Int -> Table -> Html msg
tableOption maybeId { id, name } =
    option
        [ Attr.value (toString id)
        , Attr.selected (maybeId |> Maybe.map ((==) id) |> Maybe.withDefault False)
        ]
        [ text name ]


columnSelect :
    (Reference -> msg)
    -> Reference
    -> Maybe Int
    -> List Column
    -> Html msg
columnSelect toMsg reference maybeColumnId columns =
    select
        [ EvtUtils.onChangeInt (flip Reference.selectColumn reference >> toMsg) ]
        (option [ Attr.selected (maybeColumnId == Nothing) ] [ text "Select a Column" ]
            :: List.map (columnOption maybeColumnId) columns
        )


columnOption : Maybe Int -> Column -> Html msg
columnOption maybeId { id, name } =
    option
        [ Attr.value (toString id)
        , Attr.selected (maybeId |> Maybe.map ((==) id) |> Maybe.withDefault False)
        ]
        [ text name ]


clearButton : (Reference -> msg) -> Html msg
clearButton toMsg =
    button
        [ Evt.onClick (toMsg Reference.None), Attr.type_ "button" ]
        [ text "Clear" ]
