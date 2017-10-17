module Page.Table exposing (Model, Msg, init, initialModel, update, view)

import AppUpdate exposing (AppUpdate)
import Data.ChangesetError as ChangesetError exposing (ChangesetError)
import Data.Column as Column exposing (Column, ColumnConstraints)
import Data.Combined as Combined exposing (TableWithAll)
import Data.DataType as DataType exposing (DataType)
import Data.Schema as Schema exposing (Schema)
import Data.Table as Table exposing (Table)
import Dom
import Html
    exposing
        ( Html
        , button
        , div
        , fieldset
        , form
        , h2
        , h3
        , input
        , label
        , legend
        , li
        , main_
        , option
        , p
        , section
        , select
        , span
        , text
        , ul
        )
import Html.Attributes exposing (checked, for, id, name, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Request.Column as RC
import Request.Table as RT
import Router exposing (Route)
import Task
import Utils.Handlers exposing (customOnKeyDown, onChangeInt, onEnter)
import Utils.Keys exposing (Key(..))
import Views.Breadcrumbs as BC
import Views.ChangesetError as CE
import Views.Column.ConstraintFields as CFields
import Views.Column.DataTypeDisplay as DTDisplay
import Views.Column.DataTypeSelect as DTSelect


-- MODEL


type alias Model =
    { schema : Schema
    , table : Table
    , columns : List Column
    , editingTable : Maybe Table
    , newColumn : Column
    , editingColumn : Maybe Column
    , toDeleteId : Maybe Int
    , errors : List ChangesetError
    }


initialModel : Model
initialModel =
    Model
        Schema.empty
        Table.empty
        []
        Nothing
        Column.empty
        Nothing
        Nothing
        []


type alias InitialData =
    { schema : Schema
    , table : Table
    }


init : Int -> Int -> ( Model, Cmd Msg )
init schemaId tableId =
    ( { initialModel | newColumn = Column.init tableId }
    , RT.oneWithAll tableId |> Http.toTask |> Task.attempt LoadTableWithAll
    )



-- UPDATE


type Msg
    = NoOp
    | FocusResult (Result Dom.Error ())
    | Goto Route
    | LoadTableWithAll (Result Http.Error TableWithAll)
      -- ENTITY
    | LoadTable (Result Http.Error Table)
    | EditTable
    | InputTableName String
    | CancelEditTable
    | SaveTableName
    | Destroy
    | RemoveTable (Result Http.Error ())
      -- COLUMNS
      -- CREATE COLUMN
    | UpdateNewColumn Column
    | InputNewColumnName String
    | SelectNewColumnDataType DataType
    | CreateColumn
    | LoadNewColumn (Result Http.Error Column)
      -- UPDATE COLUMN
    | EditColumn Int
    | UpdateEditingColumn Column
    | InputEditingColumnName String
    | SelectEditingColumnDataType DataType
    | CancelEditColumn
    | SaveEditingColumn
    | UpdateColumn (Result Http.Error Column)
      -- DELETE COLUMN
    | DestroyColumn Int
    | RemoveColumn (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, AppUpdate )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult (Ok ()) ->
            ( model, Cmd.none, AppUpdate.none )

        FocusResult (Err _) ->
            ( model, Cmd.none, AppUpdate.none )

        Goto route ->
            ( model
            , Router.goto route
            , AppUpdate.none
            )

        LoadTableWithAll (Ok { schema, table, columns }) ->
            ( { model
                | table = table
                , schema = schema
                , columns = columns
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadTableWithAll (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- ENTITY
        LoadTable (Ok table) ->
            ( { model
                | table = table
                , editingTable = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        LoadTable (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        EditTable ->
            ( { model
                | editingTable = Just model.table
                , editingColumn = Nothing
                , errors = []
              }
            , Dom.focus "edit-table-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        InputTableName name ->
            ( { model
                | editingTable =
                    Maybe.map (Table.updateName name) model.editingTable
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditTable ->
            ( { model | editingTable = Nothing }
            , Cmd.none
            , AppUpdate.none
            )

        SaveTableName ->
            ( model
            , model.editingTable
                |> Maybe.map (RT.update >> Http.send LoadTable)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        Destroy ->
            ( model
            , RT.destroy model.table.id |> Http.send RemoveTable
            , AppUpdate.none
            )

        RemoveTable (Ok ()) ->
            ( { model | errors = [] }
            , Router.goto (Router.Schema model.schema.id)
            , AppUpdate.none
            )

        RemoveTable (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Cmd.none
            , AppUpdate.none
            )

        -- COLUMNS
        -- NEW COLUMN
        UpdateNewColumn column ->
            ( { model | newColumn = column }
            , Cmd.none
            , AppUpdate.none
            )

        InputNewColumnName name ->
            ( { model | newColumn = Column.updateName name model.newColumn }
            , Cmd.none
            , AppUpdate.none
            )

        SelectNewColumnDataType dataType ->
            ( { model
                | newColumn =
                    Column.updateDataType dataType model.newColumn
              }
            , Cmd.none
            , AppUpdate.none
            )

        CreateColumn ->
            ( model
            , RC.create model.newColumn
                |> Http.send LoadNewColumn
            , AppUpdate.none
            )

        LoadNewColumn (Ok column) ->
            ( { model
                | columns = model.columns ++ [ column ]
                , newColumn = Column.init model.table.id
                , errors = []
              }
            , Dom.focus "create-column" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        LoadNewColumn (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "create-column" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        -- EDIT COLUMN
        EditColumn id ->
            ( { model
                | editingTable = Nothing
                , editingColumn =
                    model.columns
                        |> List.filter (.id >> (==) id)
                        |> List.head
                , errors = []
              }
            , Dom.focus "edit-column-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        UpdateEditingColumn column ->
            ( { model | editingColumn = Just column }
            , Cmd.none
            , AppUpdate.none
            )

        InputEditingColumnName name ->
            ( { model
                | editingColumn =
                    Maybe.map (Column.updateName name) model.editingColumn
              }
            , Cmd.none
            , AppUpdate.none
            )

        SelectEditingColumnDataType dataType ->
            ( { model
                | editingColumn =
                    Maybe.map (Column.updateDataType dataType) model.editingColumn
              }
            , Cmd.none
            , AppUpdate.none
            )

        CancelEditColumn ->
            ( { model
                | editingColumn = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        SaveEditingColumn ->
            ( model
            , model.editingColumn
                |> Maybe.map (RC.update >> Http.send UpdateColumn)
                |> Maybe.withDefault Cmd.none
            , AppUpdate.none
            )

        UpdateColumn (Ok column) ->
            ( { model
                | columns = List.map (Column.replaceIfMatch column) model.columns
                , editingColumn = Nothing
                , errors = []
              }
            , Cmd.none
            , AppUpdate.none
            )

        UpdateColumn (Err error) ->
            ( { model | errors = ChangesetError.parseHttpError error }
            , Dom.focus "edit-column-name" |> Task.attempt FocusResult
            , AppUpdate.none
            )

        DestroyColumn id ->
            ( { model | toDeleteId = Just id }
            , RC.destroy id |> Http.send RemoveColumn
            , AppUpdate.none
            )

        RemoveColumn (Ok ()) ->
            ( { model
                | errors = []
                , columns =
                    model.toDeleteId
                        |> Maybe.map (Column.removeFromList model.columns)
                        |> Maybe.withDefault model.columns
              }
            , Cmd.none
            , AppUpdate.none
            )

        RemoveColumn (Err error) ->
            ( { model
                | errors = ChangesetError.parseHttpError error
                , toDeleteId = Nothing
              }
            , Cmd.none
            , AppUpdate.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ breadCrumbs model.schema model.table
        , tableView model.editingTable model.table
        , createColumn model.newColumn
        , columnsView model
        ]


breadCrumbs : Schema -> Table -> Html Msg
breadCrumbs schema table =
    BC.view Goto [ BC.home, BC.schema schema, BC.table table ]



-- ENTITY VIEW


tableView : Maybe Table -> Table -> Html Msg
tableView editingTable table =
    section [] (tableChildren editingTable table)


tableChildren : Maybe Table -> Table -> List (Html Msg)
tableChildren editingTable table =
    editingTable
        |> Maybe.map editingTableChildren
        |> Maybe.withDefault (readTableChildren table)



-- READ ENTITY


readTableChildren : Table -> List (Html Msg)
readTableChildren { name } =
    [ tableName name
    , editTableNameButton
    , deleteTableButton
    ]


tableName : String -> Html Msg
tableName name =
    h2 [] [ text name ]


editTableNameButton : Html Msg
editTableNameButton =
    button [ onClick EditTable ] [ text "Edit Name" ]


deleteTableButton : Html Msg
deleteTableButton =
    button [ onClick Destroy ] [ text "Delete Table" ]



-- UPDATE ENTITY


editingTableChildren : Table -> List (Html Msg)
editingTableChildren { name } =
    [ editTableNameInput name
    , cancelEditTableButton
    , saveEditTableButton
    ]


editTableNameInput : String -> Html Msg
editTableNameInput name =
    input
        [ id "edit-table-name"
        , value name
        , onInput InputTableName
        , customOnKeyDown onTableNameKeyDown
        ]
        []


onTableNameKeyDown : Key -> Maybe Msg
onTableNameKeyDown key =
    case key of
        Enter ->
            Just SaveTableName

        Escape ->
            Just CancelEditTable

        _ ->
            Nothing


cancelEditTableButton : Html Msg
cancelEditTableButton =
    button [ onClick CancelEditTable ] [ text "Cancel" ]


saveEditTableButton : Html Msg
saveEditTableButton =
    button [ onClick SaveTableName ] [ text "Save" ]



-- CREATE COLUMN


createColumn : Column -> Html Msg
createColumn column =
    form
        []
        [ fieldset []
            [ legend [] [ text "Create a column" ]
            , p
                []
                [ newColumnInput column.name ]
            , p
                []
                [ DTSelect.view
                    "create-column-data-type"
                    SelectNewColumnDataType
                    column.dataType
                ]
            , CFields.view
                "create-column-constraints"
                UpdateNewColumn
                column
            , createColumnButton
            ]
        ]


newColumnInput : String -> Html Msg
newColumnInput name =
    label []
        [ text "Name"
        , input
            [ id "create-column"
            , value name
            , onInput InputNewColumnName
            , onEnter CreateColumn
            ]
            []
        ]


createColumnButton : Html Msg
createColumnButton =
    button [ type_ "button", onClick CreateColumn ] [ text "Create" ]



-- COLUMNS VIEW


columnsView : Model -> Html Msg
columnsView model =
    section [] (columnsChildren model)


columnsChildren : Model -> List (Html Msg)
columnsChildren model =
    CE.prependIfErrors model.errors
        [ columnsTitle
        , columnList model.editingColumn model.schema.id model.columns
        ]


columnsTitle : Html msg
columnsTitle =
    h3 [] [ text "Columns" ]


columnList : Maybe Column -> Int -> List Column -> Html Msg
columnList editingColumn schemaId columns =
    ul [] (List.map (columnItem editingColumn schemaId) columns)



-- READ COLUMN ITEM


columnItem : Maybe Column -> Int -> Column -> Html Msg
columnItem editingColumn schemaId column =
    li [] (columnItemChildren editingColumn schemaId column)


columnItemChildren : Maybe Column -> Int -> Column -> List (Html Msg)
columnItemChildren editingColumn schemaId column =
    editingColumn
        |> Maybe.map (getEditingColumnItemChildren column)
        |> Maybe.withDefault (normalColumnItemChildren schemaId column)


normalColumnItemChildren : Int -> Column -> List (Html Msg)
normalColumnItemChildren schemaId column =
    [ columnLink schemaId column
    , DTDisplay.view column.dataType
    , editColumnButton column.id
    , deleteColumnButton column.id
    ]


columnLink : Int -> Column -> Html Msg
columnLink schemaId { tableId, id, name } =
    Router.link Goto (Router.Column schemaId tableId id) [] [ text name ]


editColumnButton : Int -> Html Msg
editColumnButton id =
    button [ onClick (EditColumn id) ] [ text "Edit" ]


deleteColumnButton : Int -> Html Msg
deleteColumnButton id =
    button [ onClick (DestroyColumn id) ] [ text "Delete" ]



-- WRITE COLUMN CHILD


getEditingColumnItemChildren : Column -> Column -> List (Html Msg)
getEditingColumnItemChildren column editingColumn =
    if column.id == editingColumn.id then
        editingColumnItemChildren editingColumn
    else
        [ text column.name ]


editingColumnItemChildren : Column -> List (Html Msg)
editingColumnItemChildren column =
    [ editColumnNameInput column.name
    , DTSelect.view
        "edit-column-data-type"
        SelectEditingColumnDataType
        column.dataType
    , CFields.view
        "create-column-constraints"
        UpdateEditingColumn
        column
    , cancelEditColumnButton
    , saveEditColumnButton
    ]


editColumnNameInput : String -> Html Msg
editColumnNameInput name =
    input
        [ id "edit-column-name"
        , value name
        , onInput InputEditingColumnName
        , customOnKeyDown onColumnNameKeyDown
        ]
        []


onColumnNameKeyDown : Key -> Maybe Msg
onColumnNameKeyDown key =
    case key of
        Enter ->
            Just SaveEditingColumn

        Escape ->
            Just CancelEditColumn

        _ ->
            Nothing


cancelEditColumnButton : Html Msg
cancelEditColumnButton =
    button [ onClick CancelEditColumn ] [ text "Cancel" ]


saveEditColumnButton : Html Msg
saveEditColumnButton =
    button [ onClick SaveEditingColumn ] [ text "Save" ]
