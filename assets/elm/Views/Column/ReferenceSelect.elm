module Views.Column.ReferenceSelect exposing (view)

import Data.Column exposing (Column)
import Data.Column.Reference as Reference exposing (Reference(..))
import Data.Table exposing (Table)
import Html
    exposing
        ( Html
        , button
        , li
        , option
        , select
        , text
        )
import Html.Attributes as Attr
import Html.Events as Evt
import Utils.Events as EvtUtils


view :
    (Maybe Reference -> msg)
    -> Maybe Reference
    -> List Table
    -> List Column
    -> Maybe (Html msg)
view toMsg maybeReference tables columns =
    case ( columns, maybeReference ) of
        ( [], _ ) ->
            Nothing

        ( _, Nothing ) ->
            Just
                (button
                    [ Evt.onClick (Just Reference.start |> toMsg), Attr.type_ "button" ]
                    [ text "Add Foreign Key" ]
                )

        ( _, Just reference ) ->
            Just (createReference toMsg tables columns reference)


createReference :
    (Maybe Reference -> msg)
    -> List Table
    -> List Column
    -> Reference
    -> Html msg
createReference toMsg tables allColumns reference =
    case reference of
        SelectTable ->
            li []
                [ tableSelect toMsg reference Nothing tables
                , deleteButton toMsg
                ]

        SelectColumn tableId ->
            let
                columns =
                    List.filter (.tableId >> (==) tableId) allColumns
            in
            li []
                [ tableSelect toMsg reference (Just tableId) tables
                , columnSelect toMsg reference Nothing columns
                , deleteButton toMsg
                ]

        Ready tableId columnId ->
            let
                columns =
                    List.filter (.tableId >> (==) tableId) allColumns
            in
            li []
                [ tableSelect toMsg reference (Just tableId) tables
                , columnSelect toMsg reference (Just columnId) columns
                , deleteButton toMsg
                ]

        Display tableId _ columnId _ ->
            let
                columns =
                    List.filter (.tableId >> (==) tableId) allColumns
            in
            li []
                [ tableSelect toMsg reference (Just tableId) tables
                , columnSelect toMsg reference (Just columnId) columns
                , deleteButton toMsg
                ]


tableSelect :
    (Maybe Reference -> msg)
    -> Reference
    -> Maybe Int
    -> List Table
    -> Html msg
tableSelect toMsg reference maybeTableId tables =
    case tables of
        [] ->
            select [ Attr.disabled True ] [ option [] [ text "No for datatype" ] ]

        _ ->
            select
                [ EvtUtils.onChangeInt (flip Reference.selectTable reference >> Just >> toMsg) ]
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
    (Maybe Reference -> msg)
    -> Reference
    -> Maybe Int
    -> List Column
    -> Html msg
columnSelect toMsg reference maybeColumnId columns =
    select
        [ EvtUtils.onChangeInt (flip Reference.selectColumn reference >> Just >> toMsg) ]
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


deleteButton : (Maybe Reference -> msg) -> Html msg
deleteButton toMsg =
    button
        [ Evt.onClick (toMsg Nothing), Attr.type_ "button" ]
        [ text "Delete" ]
